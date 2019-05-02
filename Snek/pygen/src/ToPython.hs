module ToPython where

import Text.PrettyPrint -- https://hackage.haskell.org/package/pretty
import AST
import Prelude hiding ((<>))


class ToPython a where
  pprint :: a -> Doc
  toPython :: a -> String

  -- default implementation of toPython
  -- most instances do not need a specific implementation
  toPython = render . pprint

instance ToPython AddOp where
  pprint o = case o of
    Sum -> char '+'
    Sub -> char '-'

instance ToPython MulOp where
  pprint o = case o of
    Mul -> char '*'
    Div -> char '/'
    IntDiv -> text "//"
    Mod -> char '%'

instance (ToPython a) => ToPython (AEXP a) where
  pprint (AVAL a) = pprint a
  pprint (AMEXP m) = pprint m
  pprint (AEXP o e a) = (pprint e) <+> (pprint o) <+> (pprint a)

instance (ToPython a) => ToPython (MEXP a) where
  pprint (MVAL a) = pprint a
  pprint (MEEXP m) = pprint m
  pprint (MEXP o e a) = (pprint e) <+> (pprint o) <+> (pprint a)


instance ToPython Integer where
  pprint = integer


instance (ToPython a) => ToPython (EEXP a) where
  pprint (EVAL a) = pprint a
  pprint (EEXP e1 e2) = (pprint e1) <+> text "**" <+> (pprint e2)
  pprint (EPAR e) = parens $ pprint e


-- Boolean Pretty Printing

instance (ToPython a) => ToPython (OR a) where
  pprint (OVAL a) = pprint a
  pprint (OR e1 e2) = (pprint e1) <+> text "or" <+> (pprint e2)
  pprint (OAND e) = pprint e

instance (ToPython a) => ToPython (AND a) where
  pprint (ANVAL a) = pprint a
  pprint (AND e1 e2) = (pprint e1) <+> text "and" <+> (pprint e2)
  pprint (ANOT e) = pprint e

instance (ToPython a) => ToPython (NOT a) where
  pprint (NVAL a)  = pprint a
  pprint (NOT e)   = text "not" <+> (pprint e)
  pprint (NCOMP e) = pprint e 
  pprint (NPAR e)  = parens $ pprint e

instance (ToPython a) => ToPython (COMP a) where
  pprint (CVAL a) = pprint a
  pprint (COMP op e1 e2) = (pprint e1) <+> (pprint op) <+> (pprint e2)

instance ToPython CompOp where
  pprint o = case o of
    Lt  -> char '<'
    Gt  -> char '>'
    Eq  -> text "=="
    Ne  -> text "!="
    Gte -> text ">="
    Lte -> text "<="

instance ToPython Bool where
  pprint = text . show


instance (ToPython a) => ToPython (Statement a) where
  pprint (Print a)             = text "print(" <> (pprint a) <> char ')'
  pprint (If test body orelse) = (text "if") <+>
                                 (pprint test) <+>
                                 (char ':') $$ nest 4 (pprint body) $$
                                 (case orelse of
                                   Nothing -> empty
                                   Just s  -> (text "else:") $$ nest 4 (pprint s))

-- This Two Instances are mainly for debugging and have no
-- real use in sending that to the client

instance (ToPython a, ToPython b) => ToPython (Either a b) where
  pprint (Left l) = pprint l
  pprint (Right r) = pprint r

instance ToPython Hole where
  pprint _ = text "_"

