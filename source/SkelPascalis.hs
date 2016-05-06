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
transStm :: Stm -> Result
transStm x = case x of
  Skip -> failure x
  SPrint exp -> failure x
  SIf bexp stm -> failure x
  SIfElse bexp stm1 stm2 -> failure x
  SDecl decl -> failure x
  SExp exp -> failure x
  SBlock stms -> failure x
  SWhile exp stm -> failure x
  SReturn exp -> failure x
transBExp :: BExp -> Result
transBExp x = case x of
  BTrue -> failure x
  BFalse -> failure x
  BOr bexp1 bexp2 -> failure x
  BAnd bexp1 bexp2 -> failure x
  EBAss bexp1 bexp2 -> failure x
  EBNAss bexp1 bexp2 -> failure x
  EAss exp1 exp2 -> failure x
  ENAss exp1 exp2 -> failure x
  ELt exp1 exp2 -> failure x
  EGt exp1 exp2 -> failure x
  ELEt exp1 exp2 -> failure x
  EGEt exp1 exp2 -> failure x
transExp :: Exp -> Result
transExp x = case x of
  EAdd exp1 exp2 -> failure x
  ESub exp1 exp2 -> failure x
  EMul exp1 exp2 -> failure x
  EDiv exp1 exp2 -> failure x
  Call ident exps -> failure x
  EVar ident -> failure x
  EStr string -> failure x
  EInt integer -> failure x
  EDouble double -> failure x
transType :: Type -> Result
transType x = case x of
  TInt -> failure x

