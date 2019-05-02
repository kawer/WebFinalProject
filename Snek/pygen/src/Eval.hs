{-# LANGUAGE FlexibleInstances #-}
module Eval where

import AST


data PyValue
  = PyInt Integer
  | PyDouble Double
  | PyBool Bool
  deriving (Show, Eq)

class Eval a where
  -- later on, it should be a value instead of an Int
  eval :: a -> PyValue

instance Eval (AEXP Integer) where
  eval (AVAL a) = PyInt a
  eval (AMEXP m) = eval m
  eval (AEXP op l r) =
    let (PyInt left) = eval l in
      PyInt $ left `o` r
    where o = case op of
            Sum -> (+)
            Sub -> (-)


instance Eval (MEXP Integer) where
  eval x = case x of
    MVAL v -> PyInt v
    MEEXP e -> eval e
    MEXP op l r ->
      let (PyInt left) = eval l in
        o left r
      where o = case op of
                  Mul -> (\x y -> PyInt $ x * y)
                  -- this should use the correct float division
                  Div -> (\x y -> PyDouble $ (fromIntegral x) / (fromIntegral y))
                  IntDiv -> (\x y -> PyInt $ x `div` y)
                  Mod -> (\x y -> PyInt $ x `mod` y)


instance Eval (EEXP Integer) where
  eval e = case e of
    EEXP x y -> let (PyInt yval) = eval y
                    (PyInt xval) = eval x
                in
                  PyInt $ xval ^ yval
    EVAL x -> PyInt x
    EPAR x -> eval x


instance Eval (OR Bool) where
  eval e = case e of
    OR x y -> let (PyBool b) = eval x in
                PyBool $ b || y
    OAND a -> eval a
    OVAL v -> PyBool v

instance Eval (AND Bool) where
  eval e = case e of
    AND x y -> let (PyBool b) = eval x in
                PyBool $ b && y
    ANOT  a -> eval a
    ANVAL v -> PyBool v

instance Eval (NOT Bool) where
  eval e = case e of
    NOT x   -> let (PyBool b) = eval x in
                PyBool $ not b
    NCOMP c -> eval c
    NVAL  v -> PyBool v
    NPAR p      -> eval p

instance Eval (COMP Bool) where
  eval e = case e of
    CVAL a      -> PyBool a
    COMP op ax ay -> let (PyInt x) = eval ax
                         (PyInt y) = eval ay in
                       PyBool $ case op of
                                  Lt  -> x < y
                                  Gt  -> x < y
                                  Eq  -> x == y
                                  Ne  -> x /= y
                                  Gte -> x >= y
                                  Lte -> x < y

-- instance Eval (Statement PyValue) where
--   eval e = case e of
--     (Print e) -> e
--     (If test body orelse) -> let (PyBool testRes) = eval test in
--                                if testRes
--                                then eval body
--                                else (case orelse of
--                                         Nothing -> eval orelse


instance Eval (OR PyValue) where
  eval e = case e of
    (OR l r) -> let (PyBool left)  = eval l
                    (PyBool right) = r in
                 eval (OR (OVAL left) right)
      

instance Eval (AEXP PyValue) where
  eval e = case e of
    (AVAL a)      -> a
    (AMEXP m)     -> undefined -- eval m
    (AEXP op l r) ->
      case eval l of
        (PyInt left) ->
            case r of
              (PyInt i)      -> PyInt $ left `o` i
              (PyDouble d)   -> PyDouble $ (fromInteger left) `o` d 
              (PyBool True)  -> PyInt $ left `o` 1
              (PyBool False) -> PyInt $ left `o` 0
          where o :: (Num a) => a -> a -> a
                o = case op of
                      Sum -> (+)
                      Sub -> (-)
        (PyDouble left) ->
          let right = case r of
                        (PyInt i)      -> fromInteger i
                        (PyDouble d)   -> d
                        (PyBool True)  -> 1
                        (PyBool False) -> 0
          in
            PyDouble $ left `o` right
          where o = case op of
                      Sum -> (+)
                      Sub -> (-)
        (PyBool  tof) ->
          let left  = if tof then 1 else 0
          in
            case r of
              (PyInt i)      -> PyInt $ left `o` i
              (PyDouble d)   -> PyDouble $ (fromInteger left) `o` d 
              (PyBool True)  -> PyInt $ left `o` 1
              (PyBool False) -> PyInt $ left `o` 0
          where o :: (Num a) => a -> a -> a
                o = case op of
                      Sum -> (+)
                      Sub -> (-)

instance Eval (MEXP PyValue) where
  eval e = case e of
    (MVAL a)      -> a
    (MEEXP m)     -> undefined -- eval m
    (MEXP op l r) ->
      case eval l of
        (PyInt left) ->
          case r of
            (PyInt i)      -> left `o` i
            (PyDouble d)   -> case op of
                                Mul -> PyDouble $ (fromIntegral left) + d
                                Div -> PyDouble $  (fromIntegral left) + d
                                IntDiv -> error "argument error in //"
                                Mod    -> error "argument error in %"
            (PyBool True)  -> left `o` 1
            (PyBool False) -> left `o` 0
          where o = case op of
                      Mul -> (\x y -> PyInt $ x * y)
                      Div -> (\x y -> PyDouble $ (fromIntegral x) / (fromIntegral y))
                      IntDiv -> (\x y -> PyInt $ x `div` y)
                      Mod -> (\x y -> PyInt $ x `mod` y)
        (PyDouble left) ->
          let right = case r of
                        (PyInt i)      -> fromInteger i
                        (PyDouble d)   -> d
                        (PyBool True)  -> 1
                        (PyBool False) -> 0
          in
            PyDouble $ left `o` right
          where o = case op of
                      Mul -> (*)
                      Div -> (/)
                      IntDiv -> error "argument error in //"
                      Mod -> error "argument error in %"
        (PyBool  tof) ->
          let left  = if tof then 1 else 0
          in
            case r of
              (PyInt i)      -> left `o` i
              (PyDouble d)   -> case op of
                                  Mul -> PyDouble $ (fromIntegral left) + d
                                  Div -> PyDouble $  (fromIntegral left) + d
                                  IntDiv -> error "argument error in //"
                                  Mod    -> error "argument error in %"
              (PyBool True)  -> left `o` 1
              (PyBool False) -> left `o` 0
          where o = case op of
                      Mul -> (\x y -> PyInt $ x * y)
                      Div -> (\x y -> PyDouble $ (fromIntegral x) / (fromIntegral y))
                      IntDiv -> (\x y -> PyInt $ x `div` y)
                      Mod -> (\x y -> PyInt $ x `mod` y)
