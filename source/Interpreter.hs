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


-- interpret declarations
iDecl :: [Decl] -> [Stm] -> MRSIO ()
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
        return_IO
  }

iDecl [] stm = interpretStmts stm


-- interpret stmts ...
iStmt :: Stm -> MRSIO ()
iStmt Skip           = return_IO
iStmt (SPrint value) = do {
    typ <- getType value;
    result <- getConverter typ value;
    putStr_IO $ showExp result;
  }

iStmt (SExp value)   = interSExp value
  where
    interSExp (Call (Ident "incribe") list) = printParams list
      where
        printParams [] = return ()
        printParams (h:t) = do{iStmt (SPrint h); printParams t}

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
    runBlock env store interpretStmts stms;
  }

interpretStmts :: [Stm] -> MRSIO ()
interpretStmts [] = return_IO
interpretStmts ((SDecl decls):restStms) = iDecl decls restStms
interpretStmts (h:t) = do iStmt h
                          interpretStmts t


interpretProg prog = runStateT (runReaderT (interpret_ prog) M.empty) M.empty
  where
    interpret_ :: Program -> MRSIO ()
    interpret_ (Prog _ d i) = iDecl d i
