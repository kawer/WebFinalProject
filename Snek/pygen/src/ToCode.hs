{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
module ToCode where

import GHC.Generics

import AST

import Data.Either
import Data.Foldable
import ToPython

import Prelude hiding(lines)

data Fragment
  = CodeFragment String
  | CodeHole String
  deriving (Eq, Show)

isHole CodeHole{} = True
isHole _          = False

isFrag CodeFragment{} = True
isFrag _              = False

data Code = Code
  { lines :: [[Fragment]] }
  deriving (Generic, Eq, Show)


class ToCode a where
  toFragments :: a -> [Fragment]
  toCode :: a -> Code
  -- default implementation but should not be overriden generally
  toCode x = Code [filter (/=emptyFrag) $ groupFrags $ toFragments x]


groupFrags :: [Fragment] -> [Fragment]
groupFrags xs = let (frags,rest) = span isFrag xs
                    joined =     foldr joinCodeFragments emptyFrag frags in
                  case rest of
                    [] -> [joined]
                    _  -> joined : (head rest) : (groupFrags (tail rest))

emptyFrag = CodeFragment ""

joinCodeFragments :: Fragment -> Fragment -> Fragment
joinCodeFragments (CodeFragment f) (CodeFragment g) = CodeFragment (f++" "++g)

appendToCode :: Code -> String -> Code
appendToCode c s = let lns = lines c in
                     Code $ ((head lns) ++ [CodeFragment s]) : (tail lns)


instance (ToPython a) => ToCode (Either Hole a) where
  toFragments (Left h)  = [CodeHole "_"]
  toFragments (Right e) = pyFrag e

pyFrag x = [CodeFragment $ toPython x]

toFrag x = case x of
  (Left x')  -> [CodeHole $ toPython x]
  (Right x') -> pyFrag x'

instance (ToPython a) => ToCode (AEXP (Either Hole a)) where
  toFragments a =
    case a of
      (AEXP op lhs rhs) -> case lefts $ toList a of
                             [] -> pyFrag a
                             _  -> (toFragments lhs) ++ (pyFrag op) ++ (toFragments rhs)
      (AMEXP m)         -> toFragments m
      (AVAL  v)         -> toFrag v


instance (ToPython a) => ToCode (MEXP (Either Hole a)) where
  toFragments m =
    case m of
      (MEXP op lhs rhs) -> case lefts $ toList m of
                             [] -> pyFrag m
                             _  -> (toFragments lhs) ++ (pyFrag op) ++ (toFragments rhs)
      (MEEXP m)         -> toFragments m
      (MVAL  v)         -> toFrag v

instance (ToPython a) => ToCode (EEXP (Either Hole a)) where
  toFragments e =
    case e of
      (EEXP e1 e2) -> case lefts $ toList e of
                        [] -> pyFrag e
                        _  -> (toFragments e1) ++ [CodeFragment "**"] ++ (toFragments e2)
      (EVAL v)     -> toFrag v
      (EPAR p)     -> case lefts $ toList e of
                        [] -> pyFrag e
                        _  -> [CodeFragment "("] ++ (toFragments p) ++ [CodeFragment ")"]


instance (ToPython a) => ToCode (OR (Either Hole a)) where
  toFragments a =
    case a of
      (OR lhs rhs) -> case lefts $ toList a of
                        [] -> pyFrag a
                        _  -> (toFragments lhs) ++ [CodeFragment "or"] ++ (toFragments rhs)
      (OAND m)     -> toFragments m
      (OVAL v)     -> toFrag v


instance (ToPython a) => ToCode (AND (Either Hole a)) where
  toFragments a =
    case a of
      (AND lhs rhs) -> case lefts $ toList a of
                         [] -> pyFrag a
                         _  -> (toFragments lhs) ++ [CodeFragment "and"] ++ (toFragments rhs)
      (ANOT  m)     -> toFragments m
      (ANVAL v)     -> toFrag v

instance (ToPython a) => ToCode (NOT (Either Hole a)) where
  toFragments a =
    case a of
      (NOT exp) -> case lefts $ toList a of
                     [] -> pyFrag a
                     _  -> [CodeFragment "and"] ++ (toFragments exp)
      (NCOMP m)     -> toFragments m
      (NVAL  v)     -> toFrag v
      (NPAR  o)     -> [CodeFragment "("] ++ (toFragments o) ++ [CodeFragment ")"]


instance (ToPython a) => ToCode (COMP (Either Hole a)) where
  toFragments a =
    case a of
      (COMP _ _ _) -> pyFrag a
      -- case lefts $ toList a of
      --                    [] -> pyFrag a
      --                    _  -> (toFragments lhs) ++ [CodeFragment "and"] ++ (toFragments rhs)
      (CVAL v)          -> toFrag v

