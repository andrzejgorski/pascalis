

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
    | SIf Exp Stm
    | SIfElse Exp Stm Stm
    | SDecl Decl
    | SExp Exp
    | SBlock [Stm]
    | SWhile Exp Stm
    | SReturn Exp
  deriving (Eq, Ord, Show, Read)

data Exp
    = BTrue
    | BFalse
    | BNot Exp
    | EFSub Exp
    | ELSub Exp Exp
    | ERSub Exp Exp
    | ELRSub Exp Exp Exp
    | EKey Exp Exp
    | ELen Exp
    | EOrd Exp
    | EOr Exp Exp
    | EAnd Exp Exp
    | EAss Exp Exp
    | ENAss Exp Exp
    | ELt Exp Exp
    | EGt Exp Exp
    | ELEt Exp Exp
    | EGEt Exp Exp
    | EAdd Exp Exp
    | ESub Exp Exp
    | EMul Exp Exp
    | EDiv Exp Exp
    | Call Ident [Exp]
    | EStr String
    | EChar Char
    | EVar Ident
    | EInt Integer
    | EDouble Double
  deriving (Eq, Ord, Show, Read)

data Type = TInt
  deriving (Eq, Ord, Show, Read)

