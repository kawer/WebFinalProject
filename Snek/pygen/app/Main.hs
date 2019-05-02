module Main where


import Lib

import System.Environment


import Control.Monad hiding (fail)

import Data.Aeson
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L.Char8
import Data.List


data Topic
  = Identifiers
  | SimpleArithmetic
  | CombinedArithmetic
  | Boolean
  | Comparisons
  | Conditionals
  deriving (Show, Eq, Enum)

allTopics  = [Identifiers ..] -- for this to work, Identifiers needs to be the first topic declared
allTopicsN = map topicToString allTopics

data QuestionType
  = MultipleChoice
  | FillInTheBlank
  | SelectionChoice
  | Wordbank
  deriving (Show, Eq, Enum)

allQTypes  = [MultipleChoice ..] -- for this to work, MultipleChoice needs to be the first topic declared
allQTypesN = map questionTypeToString allQTypes

type InputError = String


type Combo = (Topic,QuestionType)


toTopic :: String -> Either InputError Topic
toTopic s = case s of
  "identifiers"         -> Right Identifiers
  "simple-arithmetic"   -> Right SimpleArithmetic
  "combined-arithmetic" -> Right CombinedArithmetic
  "boolean"             -> Right Boolean
  "comparisons"         -> Right Comparisons
  "conditionals"        -> Right Conditionals
  _                     -> fail $ "unknown topic: " ++ s

toType :: String -> Either InputError QuestionType
toType s = case s of
  "multiple-choice"   -> Right MultipleChoice
  "fill-in-the-blank" -> Right FillInTheBlank
  "selection-choice"  -> Right SelectionChoice
  "wordbank"          -> Right Wordbank
  _                   -> fail $ "unknown question type: " ++ s


topicToString :: Topic -> String
topicToString t = case t of
  Identifiers       -> "identifiers"
  SimpleArithmetic   -> "simple-arithmetic"
  CombinedArithmetic -> "combined-arithmetic"
  Boolean            -> "boolean"
  Comparisons        -> "comparisons"
  Conditionals       -> "conditionals"

questionTypeToString :: QuestionType -> String
questionTypeToString t = case t of
  MultipleChoice  -> "multiple-choice"
  FillInTheBlank  -> "fill-in-the-blank"
  SelectionChoice -> "selection-choice"
  Wordbank        -> "wordbank"


-- | Supported generators so far
generators :: [(Combo, IO L.ByteString)]
generators = [ ((SimpleArithmetic, MultipleChoice)   , encode <$> getSimpleMulti)
             , ((CombinedArithmetic, MultipleChoice) , encode <$> getComplexMulti)

             , ((SimpleArithmetic, SelectionChoice)  , encode <$> getSimpleSelect)
             , ((CombinedArithmetic, SelectionChoice), encode <$> getCombinedSelect)

             , ((SimpleArithmetic, FillInTheBlank)  , encode <$> getSimpleFIB)
             , ((CombinedArithmetic, FillInTheBlank), encode <$> getCombinedFIB)

             -- Boolean
             , ((Boolean, MultipleChoice)  , encode <$> getBoolMulti)
             , ((Boolean, SelectionChoice) , encode <$> getBoolSelect)
             , ((Boolean, FillInTheBlank)  , encode <$> getBoolFIB)

             -- Comparisons

             , ((Comparisons, MultipleChoice)  , encode <$> getCompsMulti)
             , ((Comparisons, SelectionChoice) , encode <$> getCompsSelect)
-- not supported yet, ((Comparisons, FillInTheBlank)  , encode <$> getCompsFIB)

             -- Conditionals
             , ((Conditionals, MultipleChoice)  , encode <$> getIfMulti)
             ]

-- | the generator that is going to be used depends only on the type and the topic
-- | if there is no Generator for the requested combination, fails with input error
toGenerator :: Topic -> QuestionType -> Either InputError (IO L.ByteString)
toGenerator qTopic qType =
  case lookup (qTopic,qType) generators of
    Just x -> return x
    Nothing ->  fail $ "there is no generator for the type: "
                ++ (questionTypeToString qType) ++ ", and topic: " ++ (topicToString qTopic)

process :: String -> String -> Either InputError (IO L.ByteString)
process topic qtype = do
  to <- toTopic topic
  ty <- toType qtype
  toGenerator to ty

help = do
  usage
  putStrLn $ topicPar ++ "\n\n" ++ qTypePar
    where paragraph  ss = intercalate "\n\t" ss
          topicPar = paragraph $ "topics:":allTopicsN
          qTypePar = paragraph $ "question types:":allQTypesN
usage = putStrLn "pygen <topic> <question type> [-h for help]"

main :: IO ()
main = do
  allargs <- getArgs
  let (flags,args) = partition (isPrefixOf "-") allargs
  case args of
    [] -> case flags of
              ["-h"] -> help
              _ -> usage
    [topic,qtype] -> (case process topic qtype of
                       Right payload -> payload >>= L.Char8.putStrLn
                       Left e -> putStrLn e)
    _                    -> putStrLn $ "expected two arguments, got: " ++ (show $ length args)
