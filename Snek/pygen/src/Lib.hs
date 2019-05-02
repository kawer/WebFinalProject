{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Lib
    (
      -- simple arithmetic topic
      getSimpleMulti
    , getSimpleSelect
    , getSimpleFIB

      -- combined arithmetic topic
    , getComplexMulti
    , getCombinedSelect
    , getCombinedFIB

      -- boolean topic (and, or, not)
    , getBoolMulti
    , getBoolSelect
    , getBoolFIB

      -- comparisons operators topic
    , getCompsMulti
    , getCompsSelect
    , getCompsFIB

      -- conditionals topic
    , getIfMulti

    ) where

import GHC.Generics

import Data.Aeson
import Data.List(partition)
import Data.Either(either)
import Data.Foldable(toList)
import Control.Monad(replicateM)

-- to avoid collision with Code.lines
import Prelude hiding (lines)

import AST
import Gen
import Eval
import ToPython
import ToCode


-- in this file all parts come together, the ast, the Generator, the evaluator and the python pretty printer

-- http://hackage.haskell.org/package/aeson-1.4.2.0/docs/Data-Aeson.html#g:1
-- the documentation says that:

-- Note [toEncoding Efficiency]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- For efficiency, we write a simple toEncoding implementation, as
-- the default version uses toJSON.


instance ToJSON Fragment where
  -- we add the "type" field to the JSON Structure, because it is a union type, so that the client can differentiate between the two
  toJSON (CodeFragment s) =
    object [ "type" .= ("code" :: String), "value" .= s]
  toJSON (CodeHole s) =
    object [ "type" .= ("hole" :: String), "value" .= s]

instance ToJSON Code where
  -- see Note [toEncoding Efficiency]
  toEncoding = genericToEncoding defaultOptions

data Payload = Payload
  { code :: Code
  , correctAnswers :: [String]
  , wrongAnswers :: [String]
} deriving (Generic, Eq, Show)

instance ToJSON Payload where
  -- see Note [toEncoding Efficiency]
  toEncoding = genericToEncoding defaultOptions

------------------------------------------------------------------

holedToCode :: ToCode a => a -> Code
holedToCode holed = toCode holed

astToCode :: ToPython a => a -> Code
astToCode ast = Code [[CodeFragment (toPython ast)]]

------------------------------------------------------------------
-- Arithmetic Payloads

-- | generates the payload for a multiple choice arithmetic payload
-- | there might be multiple correct answers, but at least one
multiPayload :: (Integer -> IO Arith) -> (Integer -> IO [Integer]) -> IO Payload
multiPayload gv gans = do
  result <- getValue
  pythonAST <- gv result
  otherAnswers <- gans result
  let (ca,wa) = partition (== result) otherAnswers
  return $ Payload (astToCode pythonAST) (map toPython $ (result:ca)) (map toPython wa)


-- | generates the payload for SimpleArith but multiple possible answers
-- | this type gives python code as options.
-- | example:
-- | Payload
--     { code = Code [[CodeFragment "23"]]
--     , correctAnswers = ["13 + 10","1 * 13","41 - 18"]
--     , wrongAnswers = ["10 + 1","-3 + 14","32 - 21"]
--     }
selectionPayload :: (Integer -> IO Arith) -> IO Payload
selectionPayload gv = do
  result <- getValue
  correctASTs <- replicateM 3 $ gv result
  incorrectASTs <- replicateM 3 $ gv (result `div` 2)
  return $ Payload (astToCode result)
    (map toPython correctASTs)
    (map toPython incorrectASTs)

-- | fill in the blank payload
-- gans should have type : ToPython a => (a -> IO [a])
fibPayload ::
  (Integer -> IO Arith) ->                 -- generator of an ast that evaluates to given Integer
  (Arith -> IO (HoledRes AEXP Integer)) -> -- generator of the Holed Arithmetic, given an Arith
  (Integer -> IO [Integer]) ->             -- generator of other answers
  IO Payload
fibPayload gv gh gans = do
  result <- getValue
  pythonAST <- gv result
  (holedAST, holedVal) <- gh pythonAST
  otherAnswers <- gans holedVal
  return $ Payload (appendToCode (holedToCode holedAST) (" = " ++ (toPython result))) [toPython holedVal] (map toPython otherAnswers)



getSimpleFIB   :: IO Payload
getSimpleFIB   = fibPayload getSimpleArith getSimpleHoledArith (getOtherAnswersArith 3)
getCombinedFIB :: IO Payload
getCombinedFIB = fibPayload getCombinedArith getCombinedHoledArith (getOtherAnswersArith 3)

getSimpleSelect :: IO Payload
getSimpleSelect = selectionPayload getSimpleArith
getCombinedSelect :: IO Payload
getCombinedSelect = selectionPayload getCombinedArith

getSimpleMulti :: IO Payload
getSimpleMulti  = multiPayload getSimpleArith (getOtherAnswersArith 3)
getComplexMulti :: IO Payload
getComplexMulti = multiPayload getCombinedArith (getOtherAnswersArith 3)


------------------------------------------------------------------

bmultiPayload :: ToPython a => (Bool -> IO a) -> IO Payload
bmultiPayload gb = do
  result <- getBool
  pythonAST <- gb result
  return $ Payload (astToCode pythonAST) [toPython result] [toPython $ not result]

bselectionPayload :: (ToPython a, Eval a) => (Bool -> IO a) -> IO Payload
bselectionPayload gb = do
  result <- getBool
  asts_a <- replicateM 3 $ gb result
  asts_b <- replicateM 3 $ gb (not result)
  let (correct, incorrect) = partition (\x -> eval x == (PyBool result)) (asts_a++asts_b)
  return $ Payload (astToCode result)
    (map toPython correct)
    (map toPython incorrect)

------------------------------------------------------------------
-- Boolean Payloads

getBoolMulti :: IO Payload
getBoolMulti = bmultiPayload getBoolArith

getBoolSelect :: IO Payload
getBoolSelect = bselectionPayload getBoolArith

bfibPayload :: IO Payload
bfibPayload = do
  result <- getBool
  pythonAST <- getBoolArith result
  (holedAST, holedVal) <- getBoolHoledArith pythonAST
  return $ Payload (appendToCode (holedToCode holedAST) (" = " ++ (toPython result))) [toPython holedVal] [toPython $ not holedVal]

getBoolFIB :: IO Payload
getBoolFIB = bfibPayload

------------------------------------------------------------------
-- Comparisons Payloads

getCompsMulti :: IO Payload
getCompsMulti = bmultiPayload getCompArith

getCompsSelect :: IO Payload
getCompsSelect = bselectionPayload getCompArith

getCompsFIB :: IO Payload
getCompsFIB = undefined

------------------------------------------------------------------
-- If / Ifelse Payloads

getIfMulti :: IO Payload
getIfMulti = do
  res <- getValue
  err <- getValue
  pythonAST <- getIfIfElse res err
  return $ Payload (astToCode pythonAST) [toPython res] [toPython $ err]
