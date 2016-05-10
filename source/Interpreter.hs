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


printDebug [] = return ()
printDebug (h:t) = do{putStr_IO h; printDebug t}

-- Type functions
getType :: EExp -> MRSIO Type
getType exp = case exp of
    BTrue        -> return TBool
    BFalse       -> return TBool
    BNot _       -> return TBool

    EOr _ _      -> return TBool
    EAnd _ _     -> return TBool
    EAss _ _     -> return TBool
    ENAss _ _    -> return TBool
    ELt _ _      -> return TBool
    EGt _ _      -> return TBool
    ELEt _ _     -> return TBool
    EGEt _ _     -> return TBool

    EAdd e _     -> getType e
    ESub _ _     -> return TInt
    EMul _ _     -> return TInt
    EDiv _ _     -> return TInt

    EInt _       -> return TInt
    EChar _      -> return TChar
    EStr _       -> return TStr
    EFSub _      -> return TStr
    ELSub _ _    -> return TStr
    ERSub _ _    -> return TStr
    ELRSub _ _ _ -> return TStr
    EKey e _     -> do con_t <- getType e
                       return $ getContainerValueType con_t
    ELen _       -> return TFunc
    EOrd _       -> return TFunc
    EArrI _      -> return (TArr TInt TInt)
    EDict _ t1 t2-> return (TDict t1 t2)
    EVar v       -> askType v
    Call (Ident "longitudo") _    -> return TInt
    Call (Ident "ord") _          -> return TInt
    Call id _    -> askType id
    EFunc _ t _ _-> return t


getContainerKeyType :: Type -> Type
getContainerKeyType TStr            = TInt
getContainerKeyType (TArr t1 t2)    = t1
getContainerKeyType (TDict t1 t2)   = t2


getContainerValueType :: Type -> Type
getContainerValueType TStr          = TChar
getContainerValueType (TArr t1 t2)  = t2
getContainerValueType (TDict t1 t2) = t2


-- converters utils functions
convertVar converter ident = do v <- askExp ident
                                converter v


intFromEInt :: EExp -> MRSIO Int
intFromEInt i = do int <- calcInt i
                   return $ fromIntegral $ int


concatenation (EStr s1) (EStr s2) = EStr (s1 ++ s2)


-- Converter functions
getConverter :: Type -> (EExp -> MRSIO EExp)
getConverter t = case t of
    TBool      -> calcBool
    TInt       -> calcExpInt
    TChar      -> calcChar
    TStr       -> calcString
    TFunc      -> calcFunc
    TArr _ _   -> calcArr
    TDict _ _  -> calcDict


calcBool :: EExp -> MRSIO EExp
calcBool exp = case exp of
    -- bool expressions
    BTrue           -> return BTrue
    BFalse          -> return BFalse
    BNot v          -> do bexp <- calcBool v
                          if bexp == BTrue then
                            return BFalse
                          else
                            return BTrue
    EAnd exp1 exp2  -> do bexp <- calcBool exp1
                          if bexp == BTrue then
                            calcBool exp2
                          else
                            return BFalse
    EOr exp1 exp2   -> do bexp <- calcBool exp1
                          if bexp == BTrue then
                            return BTrue
                          else
                            calcBool exp2
    EAss exp1 exp2  -> calcConvered (==) exp1 exp2
    ENAss exp1 exp2 -> calcConvered (/=) exp1 exp2
    ELt exp1 exp2   -> calcConvered (<)  exp1 exp2
    EGt exp1 exp2   -> calcConvered (>)  exp1 exp2
    ELEt exp1 exp2  -> calcConvered (<=) exp1 exp2
    EGEt exp1 exp2  -> calcConvered (>=) exp1 exp2
    EVar i          -> convertVar calcBool i
    e               -> do putStr_IO $ "\n" ++  show e ++ "\n"
                          return Null
  where
    calcConvered func exp1 exp2 = do {
        t1 <- getType exp1;
        t2 <- getType exp2;
        if t1 == t2 then
          let converter = getConverter t1;
          in do con1 <- converter exp1
                con2 <- converter exp2
                if func con1 con2 then
                  return BTrue
                else
                  return BFalse
        else
            -- TODO handle errors
          return BFalse
      }


calcInt :: EExp -> MRSIO Integer
calcInt x = case x of
    EVar ident     -> convertVar calcInt ident
    EAdd exp1 exp2 -> do n1 <- calcInt exp1
                         n2 <- calcInt exp2
                         return $ n1 + n2
    ESub exp1 exp2 -> do n1 <- calcInt exp1
                         n2 <- calcInt exp2
                         return $ n1 - n2
    EMul exp1 exp2 -> do n1 <- calcInt exp1
                         n2 <- calcInt exp2
                         return $ n1 * n2
    EDiv exp1 exp2 -> do n1 <- calcInt exp1
                         n2 <- calcInt exp2
                         return $ n1 `div` n2
    EInt n         -> return n
    Call id d      -> do exp <- calcFunc (Call id d)
                         case exp of
                           EInt i -> return i
                           e -> do putStr_IO $ "calc int " ++ show e
                                   return 0
    EKey cont k    -> getFromCont cont k
      where
        getFromCont (EArrI a) (EInt i1) = do i <- intFromEInt (EInt i1)
                                             exp <- return $ a ! i
                                             case exp of
                                               EInt i -> return $ toInteger i
                                               e      -> do putStr_Err "Error type error"
                                                            return 0

        getFromCont (EVar ident) key     = do a <- askExp ident
                                              getFromCont a key
        getFromCont (EDict d t1 t2) key  = if M.member key d then
              do exp <- return $ fromJust $ M.lookup key d
                 case exp of
                  EInt i -> return $ toInteger i
                  e      -> do putStr_Err $ "Error type error"
                               return 0
              else
                do putStr_Err $ "Dict " ++ show d ++ " doesn't have a key " ++ show key
                   return 0

        getFromCont arr key              = do cexp <- calcExp key
                                              getFromCont arr cexp


calcExpInt :: EExp -> MRSIO EExp
calcExpInt exp = do int <- calcInt exp
                    return $ EInt $ toInteger int


calcChar :: EExp -> MRSIO EExp
calcChar (EChar c) = return $ EChar c
calcChar (EVar i)  = convertVar calcChar i
calcChar (EKey container k) = getFromCont container k
  where
    getFromCont (EStr s) k       = do i <- intFromEInt k
                                      return $ EChar $ head $ drop i s

    getFromCont (EVar ident) key = do a <- askExp ident
                                      getFromCont a key

calcString :: EExp -> MRSIO EExp
calcString str = case str of
    EStr s              -> return $ EStr s
    EAdd s1 s2          -> do
                              str1 <- calcString s1
                              str2 <- calcString s2
                              return $ concatenation str1 str2

    EFSub s             -> return s

    ELSub (EStr s) l    -> do i <- intFromEInt l
                              return $ EStr $ drop i s
    ERSub (EStr s) l    -> do i <- intFromEInt l
                              return $ EStr $ take i s
    ELRSub (EStr s) l r -> do {
        left <- intFromEInt l;
        right <- intFromEInt r;
        return $ EStr ((take (right - left)) $ drop left s)
      }
    EVar i              -> convertVar calcString i


handleParams [] [] env = return env
handleParams (DParam id ty:tc) (exp:te) env = do {
    calced <- calcExp exp;
    tType <- getType calced;
    loc <- alloc tType;
    new_env <- return $ M.insert id loc env;
    putToStore loc calced;
    handleParams tc te new_env;
}
handleParams (DVar id1 ty:tc) (EVar id2:te) env = do {
    loc <- getLoc id2;
    new_env <- return $ M.insert id1 loc env;
    handleParams tc te new_env;
}

handleFunc :: Exp -> [Exp] -> MRSIO Exp
handleFunc (EProc decls stmts env) params = do {
    new_env <- handleParams decls params env;
    exp <- runBlock new_env interpretStmts stmts;
    return Null;
  }

handleFunc (EFunc decls tType stmts env) params = do {
    new_env <- handleParams decls params env;
    runBlock new_env interpretStmts stmts;
  }


calcFunc :: EExp -> MRSIO EExp
calcFunc (ELen (EStr s))  = return $ EInt (toInteger $ length s)
calcFunc (ELen (EArrI a))= return $ EInt (toInteger $ range_ $ bounds a)
  where
    range_ bounds = (snd bounds) - (fst bounds) + 1
calcFunc (ELen (EVar v))  = do exp <- calcExp (EVar v)
                               calcFunc (ELen exp)

calcFunc (EOrd (EChar c)) = return $ EInt (toInteger $ ord c)
calcFunc (EOrd (EVar v))  = do exp <- calcExp (EVar v)
                               calcFunc (EOrd exp)

calcFunc (Call (Ident "ord") h) = calcFunc $ EOrd $ head h
calcFunc (Call (Ident "longitudo") h) = calcFunc $ ELen $ head h
calcFunc (Call id params) = do exp <- askExp id
                               handleFunc exp params

-- Debug
calcFunc f                = do putStr_Err $ "calc func " ++ show f
                               return Null


calcArr :: EExp -> MRSIO EExp
calcArr (EVar id) = askExp id
calcArr e = return e

calcDict :: EExp -> MRSIO EExp
calcDict (EVar id) = askExp id
calcDict e = return e

calcExp :: EExp -> MRSIO EExp
calcExp e = do t <- getType e
               getConverter t e

rangeExp :: EExp -> EExp -> Int
rangeExp (EInt i1) (EInt i2) = fromInteger $ i2 - i1 + 1

nextExp :: EExp -> MRSIO EExp
nextExp (EInt i) = return (EInt (i + 1))


-- utils funcitons
showExp (EInt i)    = show i
showExp (EStr s)    = s
showExp (EChar s)   = [s]
showExp (EArrI a)  = show $ assocs a
showExp BTrue       = "verum"
showExp BFalse      = "falsum"


createArray :: EExp -> EExp -> Type -> EExp
createArray (EInt i1) (EInt i2) TInt = EArrI (array (fromInteger i1, fromInteger i2) [])


update_container :: Exp -> Exp -> Exp -> Exp
update_container (EStr s) (EInt i) (EChar c) = EStr (upd_con s i c)
  where
    upd_con (_:t) 0 c = (c:t)
    upd_con (h:t) i c = (h:upd_con t (i - 1) c)

update_container (EDict d t1 t2) e1 e2 = EDict (M.insert e1 e2 d) t1 t2
update_container (EArrI a) (EInt i1) (EInt i2) = EArrI (a // [(fromInteger i1, EInt i2)])

createProcedure :: [Decl] -> [Stm] -> Env -> Exp
createProcedure params stmts env = (EProc params stmts env)

createFunction :: [Decl] -> Type -> [Stm] -> Env -> Exp
createFunction params tType stmts env = (EFunc params tType stmts env)


-- interpret declarations

iDecl :: [Decl] -> [Stm] -> MRSIO (Env, Exp)
iDecl ((DParam ind ty):tail) stm = iDecl ((DVar ind ty):tail) stm

iDecl ((DVar ind tType):tail) stm = do {
    loc <- alloc tType;
    case tType of
      TDict t1 t2 -> do putToStore loc (EDict M.empty t1 t2)
                        localEnv (M.insert ind loc) (iDecl tail stm)
      t         -> localEnv (M.insert ind loc) (iDecl tail stm)
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
      do  -- TODO handle error here
        env <- askEnv;
        return (env, Null);
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
    loc <- alloc TFunc;
    recursion_env <- return $ M.insert ident loc envParams;
    putToStore loc (createFunction params tType ((SDecl decl):stmts) recursion_env);
    localEnv (M.insert ident loc) (iDecl tail stm);
  }
  where
    -- TODO fix id
    addParamsToEnv decl = iDecl decl []


iDecl [] stm = do exp <- interpretStmts stm
                  env <- askEnv
                  return (env, exp)


printParams [] = return ()
printParams (h:t) = do {
    exp <- calcExp h;
    putStr_IO $ showExp exp;
    printParams t;
  }


readVariable :: Type -> MRSIO Exp
readVariable TStr  = do s <- getLine_IO
                        return $ EStr s
readVariable TChar = do c <- getChar_IO
                        return $ EChar c
readVariable TInt = get_ 0 1
  where
    get_ :: Int -> Int -> MRSIO Exp
    get_ 0 n = do c <- getChar_IO
                  if isSpace c then
                    get_ 0 n
                  else
                   if c == '-' then
                     get_ 0 (n * (- 1))
                   else
                    if isDigit c then
                      get_ (ord c - 48) n
                    else
                      do putStr_Err $
                           "Read function error. Cannot read type: "
                           ++ show TInt ++ "  " ++ show c ++ " on input";
                         return Null

    get_ i n = do c <- getChar_IO
                  if isDigit c then
                    get_ ((i * 10) + (ord c - 48)) n
                  else
                   if isSpace c then
                     return $ EInt $ toInteger (i * n)
                   else
                    do putStr_Err $
                           "Read function error. Cannot read type: "
                           ++ show TInt ++ "  " ++ show c ++ " on input";
                       return Null


-- interpret stmts ...
iStmt :: Stm -> MRSIO Exp
iStmt Skip           = return Null
iStmt (SReturn exp)  = do calced <- calcExp exp
                          return calced
iStmt (SExp value)   = do interSExp value
  where
    interSExp (Call (Ident "incribo") params) = do printParams params
                                                   return Null
    interSExp (Call ident params) = do exp <- askExp ident
                                       result <- handleFunc exp params
                                       return Null


iStmt (SIf exp stm)  = do {
    bexp <- calcBool exp;
    if bexp ==  BTrue then
        iStmt stm;
    else
        return Null;
  }

iStmt (SIfElse exp stm1 stm2) = do {
    bexp <- calcBool exp;
    if bexp == BTrue then
        iStmt stm1;
    else
        iStmt stm2;
  }
iStmt (SSet ident (Call (Ident "lege") params)) = do printParams params
                                                     t <- askType ident
                                                     exp <- readVariable t
                                                     iStmt (SSet ident exp)

iStmt (SSet ident value) = do t1 <- askType ident
                              t2 <- getType value
                              if t2 == TFunc then
                                do exp <- calcExp value
                                   t3 <- getType exp
                                   if t1 == t3 then
                                     do loc <- getLoc ident
                                        putToStore loc exp
                                        return Null
                                   else
                                     do putStr_Err $ show t3 ++ " cannot be set to " ++ show t1
                                        return Null
                              else
                                if t1 == t2 then
                                 do loc <- getLoc ident
                                    -- Debug
                                    exp <- calcExp value
                                    putToStore loc exp
                                    return Null
                                else
                                    do putStr_IO $ show t2 ++ " cannot be set to " ++ show t1
                                       return Null

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
         return Null
    else
        -- TODO err
        return Null
  }

iStmt (SBlock stms) = do {
    env <- askEnv;
    store <- getStore;
    runBlock env interpretStmts stms;
  }

iStmt (SWhile exp stm) = do calced <- calcBool exp
                            if calced == BTrue then
                              do e <- iStmt stm
                                 case e of
                                   Null -> iStmt (SWhile exp stm)
                                   e -> return e
                            else
                              return Null

iStmt (SFor ident exp1 exp2 stm) = do iStmt (SSet ident exp1)
                                      cexp1 <- calcExp exp1
                                      cexp2 <- calcExp exp2
                                      doNTimes stm cexp1 (rangeExp cexp1 cexp2)
                                        where
                                          doNTimes stmt old 0 = return Null
                                          doNTimes stmt old times = do {
    next <- nextExp old;
    exp <- iStmt stmt;
    case exp of
    Null -> do iStmt (SSet ident next);
               doNTimes stmt next (times - 1)
    e -> return e
}


interpretStmts :: [Stm] -> MRSIO Exp
interpretStmts [] = return Null
interpretStmts ((SDecl decls):restStms) = do env_exp <- iDecl decls restStms
                                             return $ snd env_exp
interpretStmts (h:t) = do exp <- iStmt h
                          case exp of
                            Null -> interpretStmts t;
                            otherwise -> return exp;


interpretProg prog = runStateT (runReaderT (interpret_ prog) M.empty) M.empty
  where
    interpret_ :: Program -> MRSIO ()
    interpret_ (Prog _ d i) = do env <- iDecl d i
                                 return_IO
