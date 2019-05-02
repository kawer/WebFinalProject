{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFunctor #-}
module AST where

-- Python 3.7 Reference Manual regarding expressions
-- https://docs.python.org/3/reference/expressions.html#binary-arithmetic-operations
-- observations:
-- o_expr is for binary |, or_test is for bools,
-- same for and, xor, etc.

-- precedence is stored as a layer of types,
-- Addition and Substraction have lower precedence than Multipication.
-- Therefore, they are higher up in the AST,
-- if Sum has lower precedence than Mult, Sum is in a higher structure.
-- this is to assure that the mult finishes lower down the AST, and is reduced first.

-- for example: `8 - 3 - 5` is associative to the left, therefore the tree is:
--       -
--     -   5
--   8   3

type CompArith = COMP Bool
type BoolArith = OR Bool

-- TODO: Change it to PyValue
type Arith = AEXP Integer

data OR a
  = OR (OR a) a
  | OAND (AND a)
  | OVAL a
  deriving (Show, Eq, Functor, Foldable, Traversable)

data AND a
  = AND (AND a) a
  | ANOT (NOT a)
  | ANVAL a
  deriving (Show, Eq, Functor, Foldable, Traversable)

data NOT a
  = NOT (NOT a)
  | NCOMP (COMP a)
  | NVAL a
  | NPAR (OR a)
  deriving (Show, Eq, Functor, Foldable, Traversable)

data COMP a
  = COMP CompOp Arith Arith
  | CVAL a
  deriving (Show, Eq, Functor, Foldable, Traversable)

         --   <  | >  | == | >=  | <=  | !=
data CompOp = Lt | Gt | Eq | Gte | Lte | Ne deriving (Show, Eq)

data AddOp = Sum | Sub deriving (Show, Eq)

data MulOp = Mul | Div | IntDiv | Mod deriving (Show, Eq)

-- everything can be inside AddBinExp
-- AddBinExp cannot be inside MulBinExp
-- AddBinExp and MulBinExp cannot be inside ExpoExp

data AEXP a
  = AEXP AddOp (AEXP a) a -- Left  Assoc e.g. a + b + c => (a + b) + c
  | AMEXP (MEXP a)
  | AVAL a
  deriving (Show, Eq, Functor, Foldable, Traversable)

data MEXP a
  = MEXP MulOp (MEXP a) a -- Left  Assoc e.g.   a / b / c   => (a / b) / c
  | MEEXP (EEXP a)
  | MVAL a
  deriving (Show, Eq, Functor, Foldable, Traversable)

data EEXP a
  = EEXP (EEXP a) (EEXP a)       -- Right Assoc e.g.   a ** b ** c => a ** (b ** c)
  | EVAL a
  | EPAR (AEXP a)
  -- this should contain the alternative of a Parenthesized Expression
  deriving (Show, Eq, Functor, Foldable, Traversable)


data Statement a
  = Print a
  | If (OR Bool) (Statement (AEXP a)) (Maybe (Statement (AEXP a)))
  deriving (Show, Eq, Functor, Foldable, Traversable)

-- | Hole <hole id>
data Hole = Hole Int
  deriving (Show, Eq)
