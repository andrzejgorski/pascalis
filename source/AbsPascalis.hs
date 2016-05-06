

module AbsPascalis where

-- Haskell module generated by the BNF converter




newtype Ident = Ident String deriving (Eq, Ord, Show, Read)
data Program = Prog Ident [Decl] [Stm]
  deriving (Eq, Ord, Show, Read)

data Decl = DVar Ident Type
  deriving (Eq, Ord, Show, Read)

data Stm
    = Skip
    | SPrint Exp
    | SIf BExp Stm
    | SIfElse BExp Stm Stm
    | SDecl Decl
    | SExp Exp
    | SBlock [Stm]
    | SWhile Exp Stm
    | SReturn Exp
  deriving (Eq, Ord, Show, Read)

data BExp
    = BTrue
    | BFalse
    | BOr BExp BExp
    | BAnd BExp BExp
    | EBAss BExp BExp
    | EBNAss BExp BExp
    | EAss Exp Exp
    | ENAss Exp Exp
    | ELt Exp Exp
    | EGt Exp Exp
  deriving (Eq, Ord, Show, Read)

data Exp
    = EAdd Exp Exp
    | ESub Exp Exp
    | EMul Exp Exp
    | EDiv Exp Exp
    | Call Ident [Exp]
    | EVar Ident
    | EStr String
    | EInt Integer
    | EDouble Double
  deriving (Eq, Ord, Show, Read)

data Type = TInt
  deriving (Eq, Ord, Show, Read)

