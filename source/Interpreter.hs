module Interpreter where

import Data.Array
import Data.Char
import Data.Maybe(fromMaybe, fromJust)
import qualified Data.Map as M

import Control.Monad.Reader
import Control.Monad.State

import AbsPascalis
import PrintPascalis
import ErrM

import MonadsFunctions
import Converters


-- utils funcitons
showExp (EInt i)    = show i
showExp (EStr s)    = s
showExp (EChar s)   = [s]
showExp BTrue       = "verum"
showExp BFalse      = "falsum"


createArray :: EExp -> EExp -> Type -> EExp
createArray (EInt i1) (EInt i2) TInt = EArrII (array (fromInteger i1, fromInteger i2) [])


update_container :: EExp -> EExp -> EExp -> EExp
update_container (EStr s) (EInt i) (EChar c) = EStr (upd_con s i c)
  where
    upd_con (_:t) 0 c = (c:t)
    upd_con (h:t) i c = (h:upd_con t (i - 1) c)

update_container (EArrII a) (EInt i1) (EInt i2) = EArrII (a // [(fromInteger i1, fromInteger i2)])


createProcedure :: [Decl] -> [Stm] -> Env -> Exp
createProcedure params stmts env = (EProc params stmts env)

createFunction :: [Decl] -> Type -> [Stm] -> Env -> Exp
createFunction params tType stmts env = (EFunc params tType stmts env)


-- interpret declarations

iDecl :: [Decl] -> [Stm] -> MRSIO Env
iDecl ((DParam ind ty):tail) stm = iDecl ((DVar ind ty):tail) stm

iDecl ((DVar ind ty):tail) stm = do {
    loc <- alloc ty;
    localEnv (M.insert ind loc) (iDecl tail stm);
  }

iDecl ((DAVar i e1 e2 t):tail) stm = do {
    t1 <- getType e1;
    t2 <- getType e2;
    if t1 == t2 then
      do
        loc <- alloc (TArr t1 t);
        exp1 <- calcExp e1;
        exp2 <- calcExp e2;
        putToStore loc (createArray e1 e2 t);
        localEnv (M.insert i loc) (iDecl tail stm);
    else
        -- TODO handle error here
        askEnv
  }

iDecl ((DProc ident params decl stmts):tail) stm = do {
    env <- askEnv;
    envParams <- runNewEnv env addParamsToEnv params ;
    envParamsDecls <- runNewEnv envParams addDeclsToEnv decl;
    loc <- alloc TFunc;
    recursion_env <- return $ M.insert ident loc envParamsDecls;
    putToStore loc (createProcedure params stmts recursion_env);
    localEnv (M.insert ident loc) (iDecl tail stm);
  }
  where
    -- TODO fix id
    addParamsToEnv decl = iDecl decl []
    addDeclsToEnv decl = iDecl decl []

iDecl ((DFunc ident params tType decl stmts):tail) stm = do {
    env <- askEnv;
    envParams <- runNewEnv env addParamsToEnv params ;
    envParamsDecls <- runNewEnv envParams addDeclsToEnv decl;
    loc <- alloc TFunc;
    recursion_env <- return $ M.insert ident loc envParamsDecls;
    putToStore loc (createFunction params tType stmts recursion_env);
    localEnv (M.insert ident loc) (iDecl tail stm);
  }
  where
    -- TODO fix id
    addParamsToEnv decl = iDecl decl []
    addDeclsToEnv decl = iDecl decl []


iDecl [] stm = do interpretStmts stm
                  askEnv


printParams [] = return ()
printParams (h:t) = do {
    exp <- calcExp h;
    putStr_IO $ showExp exp;
    printParams t;
  }


-- interpret stmts ...
iStmt :: Stm -> MRSIO ()
iStmt Skip           = return_IO

iStmt (SExp value)   = interSExp value
  where
    interSExp (Call (Ident "incribo") params) = printParams params
    interSExp (Call ident params) = do exp <- askExp ident
                                       result <- handleFunc exp params
                                       return_IO
      where


iStmt (SIf exp stm)  = do {
    bexp <- calcBool exp;
    if bexp ==  BTrue then
        iStmt stm;
    else
        return_IO;
  }

iStmt (SIfElse exp stm1 stm2) = do {
    bexp <- calcBool exp;
    if bexp == BTrue then
        iStmt stm1;
    else
        iStmt stm2;
  }

iStmt (SSet ident value) = do t1 <- askType ident
                              t2 <- getType value
                              if t1 == t2 then
                               do loc <- getLoc ident
                                  exp <- calcExp value
                                  putToStore loc exp
                              else
                                  -- TODO err
                                  return_IO

iStmt (STSet ident key value) = do {
    cont_t <- askType ident;
    value_t <- getType value;
    key_t <- getType key;
    if value_t == getContainerValueType cont_t && key_t == getContainerKeyType cont_t then
      do loc <- getLoc ident
         container <- askExp ident
         value_exp <- calcExp value
         key_exp <- calcExp key
         putToStore loc (update_container container key_exp value_exp)
    else
        -- TODO err
        return_IO
  }

iStmt (SBlock stms) = do {
    env <- askEnv;
    store <- getStore;
    runBlock env interpretStmts stms;
  }

iStmt (SWhile exp stm) = do calced <- calcBool exp
                            if calced == BTrue then
                              do iStmt stm
                                 iStmt (SWhile exp stm)
                            else
                              return_IO

iStmt (SFor ident exp1 exp2 stm) = do iStmt (SSet ident exp1)
                                      cexp1 <- calcExp exp1
                                      cexp2 <- calcExp exp2
                                      doNTimes stm cexp1 (rangeExp cexp1 cexp2)
                                        where
                                          doNTimes stmt old 0 = return_IO
                                          doNTimes stmt old times = do {
    next <- nextExp old;
    iStmt stmt;
    iStmt (SSet ident next);
    doNTimes stmt next (times - 1)
}


interpretStmts :: [Stm] -> MRSIO ()
interpretStmts [] = return_IO
interpretStmts ((SDecl decls):restStms) = do iDecl decls restStms
                                             return_IO
interpretStmts (h:t) = do iStmt h
                          interpretStmts t


interpretProg prog = runStateT (runReaderT (interpret_ prog) M.empty) M.empty
  where
    interpret_ :: Program -> MRSIO ()
    interpret_ (Prog _ d i) = do env <- iDecl d i
                                 return_IO
