module Gen where

import AST
import Test.QuickCheck
import Control.Applicative
import Data.Traversable
import Data.Foldable
import Data.Maybe
import Eval

-- Note [Generators for a Value]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- the algorithm goes as this:
-- given value to generate `v`,
-- 1. generate a random value y
-- 2. generate a random operation `<op`.
--
-- that represents this equation:
-- x <op> y = v
--
-- 3. for this equation, and depending on the operation solve:
--
-- for + :
--  x = v - y

-- for -:
--  x = v + y
--
-- for *:
--  x = v / y

-- for /:
--  x = v * y

-- for //:
--  x = v * y + n where n = random between 1 and (y-1)

-- for %: (but y needs to be bigger than v)
--  x = v + n*y
-- where n is an arbitrary number
-- for simplicity's sake, in mod and int div, n is 1

-- for **:
-- x = v**(1/y)
-- here we solve for y, because it is right associative, so we generate the left value first

-- for `or`:
-- x = if v then randBool, else False
-- x | y | v
-- T | T | T
-- T | F | T
-- F | T | T
-- F | F | F

-- for `and`:
-- x = if v then True, else randBool
-- x | y | v
-- T | T | T
-- T | F | F
-- F | T | F
-- F | F | F

-- for `not`:
-- x = not v
-- note: not is unary so there is no y

-- for `<`:
-- x = if v then y - d, else y + (d-1)
-- for `>`:
-- x = if v then y + d, else y - (d-1)
-- for `==`:
-- x = if v then y else (y-d) || (y+d)
-- for `>=`:
-- x = if v then y + (d-1), else y - d
-- for `<=`:
-- x = if v then y - (d-1), else y + d
-- for `!=`:
-- x = if v then y-d || y+d, else y
-- where d = random positive number > 0


-- 4. with the equation solved, calculate value x
-- 5. Generate an expression for the value x


-- Helpers

-- | a positive Int between 1 and 10
ipos :: (Integral a) => Gen a
ipos = (+1) <$> (arbitrarySizedNatural `suchThat` (<=9))

dpos :: (Fractional f, Ord f) => Gen f
dpos = arbitrarySizedFractional `suchThat` (>0)

genValue :: Gen Integer
genValue = choose (0,50)

genBool :: Gen Bool
genBool = arbitrary

-- AST Generators

genAddOp = elements [Sum, Sub]

genMulOp = elements [Mul, Div, IntDiv, Mod]

zeroExprErr = error "Expressions should have at least one expression !"

-- | `n` is number of operators, `value` is target value
genAexp :: Int -> Integer -> Gen (AEXP Integer)
genAexp n _ | n < 0 = zeroExprErr
genAexp 0 value = return $ AVAL value
genAexp n value =oneof [aGen, AMEXP <$> genMexp n value]
  where aGen = do
          y <- ipos
          op <- genAddOp
          let xValue = case op of
                         Sum -> value - y
                         Sub -> value + y
          xArith <- genAexp (n-1) xValue
          return $ AEXP op xArith y

-- | `n` is number of operators, `value` is target value
genMexp :: Int -> Integer -> Gen (MEXP Integer)
genMexp n _ | n < 0 = zeroExprErr
genMexp 0 value = return $ MVAL value
genMexp n value = oneof [mGen, MEEXP <$> genEexp n value]
  where mGen = do
          y <- ipos `suchThat` (/=0)
          op <- genMulOp
          let xValue = case op of
                -- this is not entirely correct, becase the correct one is float division
                Mul    -> value `div` y -- should be: Mult -> v / y
                Div    -> value * y
                IntDiv -> (value * y) +1
                Mod    -> value + y
          xArith <- genMexp (n-1) xValue
          return $ MEXP op xArith y

-- | `n` is number of operators, `value` is target value
-- this function tries to make a perfect square root from the value given,
-- if it fails, it creates a parenthesized expression
-- e.g.
-- > λ> generate $ genEexp 2 10
-- > EPAR (AEXP Sub (AVAL 22) 12) -- "22 - 12"
-- 
-- > λ> generate $ genEexp 2 16
-- > EEXP (EVAL 4) (EVAL 2) -- "4 ** 2"
genEexp :: Int -> Integer -> Gen (EEXP Integer)
genEexp n _ | n < 0 = zeroExprErr
genEexp 0 value = return $ EVAL value
genEexp n value = do
  let xValue = truncate $ sqrt (fromIntegral value)
  if (xValue * xValue) == value then do
    x <- genEexp (n-1) xValue
    return $ EEXP x (EVAL 2)
    else (EPAR <$> genAexp n value)

-- | generates an arbitrary Arithmetic Expression
genArith :: Int -> Integer -> Gen Arith
genArith = genAexp

---- Boolean Arithmetic

genCompOp :: Gen CompOp
genCompOp = elements [Lt, Gt, Eq, Gte, Lte, Ne]

genComp :: Int -> Bool -> Gen (COMP Bool)
genComp n _ | n < 0 = zeroExprErr
genComp 0 value = return $ CVAL value
genComp n value = do
          y  <- ipos
          d  <- ipos `suchThat` (/=0)
          ny <- elements [y - d, y + d]
          op <- genCompOp
          let x = case op of
                    Lt  -> if value then y - d else y + (d-1)
                    Gt  -> if value then y + d else y - (d-1)
                    Eq  -> if value then y else ny
                    Ne  -> if value then ny else y
                    Gte -> if value then y + (d-1) else y - d
                    Lte -> if value then y - (d-1) else y + d
          xArith <- genAexp (n `div` 2) x
          yArith <- genAexp (n `div` 2) y
          return $ COMP op xArith yArith


genNot :: Int -> Bool -> Gen (NOT Bool)
genNot n _ | n < 0 = zeroExprErr
genNot 0 value = return $ NVAL value
genNot n value = NOT <$> genNot (n-1) (not value)
-- in genNot we avoid the possible NCOMP, so that boolean questions
-- do no contain comparisons

genAnd :: Int -> Bool -> Gen (AND Bool)
genAnd n _ | n < 0 = zeroExprErr
genAnd 0 value = return $ ANVAL value
genAnd n value = oneof [aGen, ANOT <$> genNot n value]
  where aGen = do
          y <- arbitrary :: (Gen Bool)
          x <- if value
               then return True
               else arbitrary
          xArith <- genAnd (n-1) x
          return $ AND xArith y

genOr :: Int -> Bool -> Gen (OR Bool)
genOr n _ | n < 0 = zeroExprErr
genOr 0 value = return $ OVAL value
genOr n value = oneof [oGen, OAND <$> genAnd n value]
  where oGen = do
          y <- arbitrary :: (Gen Bool)
          x <- if value
               then arbitrary
               else return False
          xArith <- genOr (n-1) x
          return $ OR xArith y

genBoolArith :: Int -> Bool -> Gen BoolArith
genBoolArith = genOr

genCompArith :: Int -> Bool -> Gen CompArith
genCompArith = genComp


---- If/ IfElse expressions

genIf :: Integer -> Integer -> Gen (Statement Integer)
genIf res err = do
  -- condition
  cond <- genBool
  test <- genOr 2 cond

  -- body and else block
  coinflip <- genBool
  x <- genArith 2 res
  y <- genArith 2 err
  let (body, orelse) = if coinflip
                       then (x,y) else (y,x)
  return $ If test (Print body) (Just $ Print orelse)

---- Holed Arithmetic Expressions

indexExp :: (Traversable t) => t a -> t (a, Int)
indexExp original = snd $ mapAccumL (\x y -> (x+1, (y,x))) 0 original

getAt :: (Foldable t) => Int -> t (a, Int) -> a
getAt i exp = fst $ fromJust $ find (\(value, index) -> index == i-1) exp

mutateRandomElem :: Traversable t => (b -> a) -> t b -> Gen (t (Either a b), b)
mutateRandomElem f original = do
  k <- choose (0, length original)
  return undefined
  return $ (fmap (mutate k) $ indexExp original, getAt k $ indexExp original)
    where mutate k (value,index) =
            if index == k-1
            then Left (f value)
            else Right value


-- a tuple with the holed arith, and the holed value
type HoledRes a b = (a (Either Hole b), b)

genHoledArith :: (Traversable t) => Int -> Int -> t b -> Gen (t (Either Hole b), b)
genHoledArith size holes a
  | (size+1) < holes = error "cannot have more holes than operands"
  | holes > 1 = error "multiple holes not implemented yet"
  | otherwise = do
      mutateRandomElem (\_ -> Hole 0) a

---------


-- | receives number of other answers and correct answer. returns other answers,
-- it does not check that other answers are unique, or wrong.
--
-- current strategy for generating other answers is really simple:
-- add to the correct answer a random value between -3 and 3.
--
-- possible extensions are:
-- different strategies per topic
-- change one operator, change precedence, etc.
genOtherAnswers :: Int -> Integer -> Gen [Integer]
-- ans should change from Int To Value when we generalize it
genOtherAnswers n ans = (map (\x -> ans + x)) <$> (vectorOf n $ deviation)
  where deviation = choose  (-3,3) `suchThat` (/= 0)


-- gets are on IO Monad, in constrast with gen, that is on the Gen Monad


--- Arithmetic

getOtherAnswersArith :: Int -> Integer -> IO [Integer]
getOtherAnswersArith n ans = generate $ genOtherAnswers n ans

-- | makes a hole in Arith a
getSimpleHoledArith   :: Arith -> IO (HoledRes AEXP Integer)
getSimpleHoledArith   a = generate $ genHoledArith 2 1 a

-- | makes a hole in Arith a
getCombinedHoledArith :: Arith -> IO (HoledRes AEXP Integer)
getCombinedHoledArith a = generate $ genHoledArith 3 1 a

-- | makes an arithmetic expression that equals number a
getSimpleArith   :: Integer -> IO Arith
getSimpleArith   a = generate $ genArith 2 a

-- | makes an arithmetic expression that equals number a
getCombinedArith :: Integer -> IO Arith
getCombinedArith a = generate $ genArith 3 a


--- Boolean (And,Or,Not)

getBoolHoledArith :: BoolArith -> IO (HoledRes OR Bool)
getBoolHoledArith a = generate $ genHoledArith 2 1 a

getBoolArith :: Bool -> IO BoolArith
getBoolArith a = generate $ genBoolArith 1 a

-- Comparison Operators

getCompArith :: Bool -> IO CompArith
getCompArith a = generate $ genCompArith 2 a

-- If / If else

getIfIfElse :: Integer -> Integer -> IO (Statement Integer)
getIfIfElse correct incorrect = generate $ genIf correct incorrect

--- Utilies

getValue = generate genValue
getBool = generate $ genBool
