module SkelPascalis where

-- Haskell module generated by the BNF converter

import AbsPascalis
import ErrM
type Result = Err String

failure :: Show a => a -> Result
failure x = Bad $ "Undefined case: " ++ show x

transIdent :: Ident -> Result
transIdent x = case x of
  Ident string -> failure x
transProgram :: Program -> Result
transProgram x = case x of
  Prog ident decls stms -> failure x
transDecl :: Decl -> Result
transDecl x = case x of
  DVar ident type_ -> failure x
  DAVar ident exp1 exp2 type_ -> failure x
  DProc ident decls1 decls2 stms -> failure x
transStm :: Stm -> Result
transStm x = case x of
  Skip -> failure x
  SPrint exp -> failure x
  SIf exp stm -> failure x
  SIfElse exp stm1 stm2 -> failure x
  SExp exp -> failure x
  SDecl decls -> failure x
  SBlock stms -> failure x
  SReturn exp -> failure x
  STSet ident exp1 exp2 -> failure x
  SSet ident exp -> failure x
  SWhile exp stm -> failure x
  SFor ident exp1 exp2 stm -> failure x
transExp :: Exp -> Result
transExp x = case x of
  BTrue -> failure x
  BFalse -> failure x
  Null -> failure x
  BNot exp -> failure x
  EFSub exp -> failure x
  ELSub exp1 exp2 -> failure x
  ERSub exp1 exp2 -> failure x
  ELRSub exp1 exp2 exp3 -> failure x
  EKey exp1 exp2 -> failure x
  ELen exp -> failure x
  EOrd exp -> failure x
  EOr exp1 exp2 -> failure x
  EAnd exp1 exp2 -> failure x
  EAss exp1 exp2 -> failure x
  ENAss exp1 exp2 -> failure x
  ELt exp1 exp2 -> failure x
  EGt exp1 exp2 -> failure x
  ELEt exp1 exp2 -> failure x
  EGEt exp1 exp2 -> failure x
  EAdd exp1 exp2 -> failure x
  ESub exp1 exp2 -> failure x
  EMul exp1 exp2 -> failure x
  EDiv exp1 exp2 -> failure x
  Call ident exps -> failure x
  EStr string -> failure x
  EChar char -> failure x
  EVar ident -> failure x
  EInt integer -> failure x
  EDouble double -> failure x
transType :: Type -> Result
transType x = case x of
  TInt -> failure x
  TBool -> failure x
  TStr -> failure x
  TChar -> failure x
  TFunc -> failure x
  TProc -> failure x
  TArr type_1 type_2 -> failure x
  TDict type_1 type_2 -> failure x

