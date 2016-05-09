

module AbsPascalis where
import Data.Array
type ArrII = Array Int Int


-- Haskell module generated by the BNF converter




newtype Ident = Ident String deriving (Eq, Ord, Show, Read)
data Program = Prog Ident [Decl] [Stm]
  deriving (Eq, Ord, Show, Read)

data Decl = DVar Ident Type | DAVar Ident Exp Exp Type
  deriving (Eq, Ord, Show, Read)

data Stm
    = Skip
    | SPrint Exp
    | SIf Exp Stm
    | SIfElse Exp Stm Stm
    | SExp Exp
    | SDecl [Decl]
    | SBlock [Stm]
    | SReturn Exp
    | STSet Ident Exp Exp
    | SSet Ident Exp
    | SWhile Exp Stm
    | SFor Ident Exp Exp Stm
  deriving (Eq, Ord, Show, Read)

data Exp
    = BTrue
    | BFalse
    | Null
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
    | EArrII ArrII

    | EDouble Double
  deriving (Eq, Ord, Show, Read)

data Type
    = TInt
    | TBool
    | TStr
    | TChar
    | TFunc
    | TArr Type Type
    | TDict Type Type
  deriving (Eq, Ord, Show, Read)

