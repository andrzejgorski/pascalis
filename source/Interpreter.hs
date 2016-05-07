module Interpreter where

import AbsPascalis
import PrintPascalis
import ErrM
import Data.Char

import qualified Data.Map as M
import Control.Monad.Reader
import Data.Maybe(fromMaybe, fromJust)
import Control.Monad.State
import Data.Array


type EExp = Exp

type Var = String
type Loc = Int
type Env = M.Map Ident Loc
type Store = M.Map Loc (Type, EExp)
type MRSIO a = ReaderT Env (StateT Store IO) a


return_IO :: MRSIO ()
return_IO = lift $ lift $ return ()

putStr_IO :: String -> MRSIO ()
putStr_IO s = lift $ lift $ putStr s

askEnv :: MRSIO Env
askEnv = ask

printEnv = do env <- askEnv
              putStr_IO $ show env

getLoc :: Ident -> MRSIO Loc
getLoc v = do env <- askEnv
              return $ fromJust $ M.lookup v env

localEnv :: (Env -> Env) -> (MRSIO () -> MRSIO ())
localEnv f s = local f s

getExpFromStore :: Loc -> MRSIO EExp
getExpFromStore l = do {
    store <- getStore;
    return $ snd $ fromJust $ M.lookup l store;
  }

askExp :: Ident -> MRSIO EExp
askExp ident = do loc <- getLoc ident
                  getExpFromStore loc

getTypeFromStore :: Loc -> MRSIO Type
getTypeFromStore l = do {
    store <- getStore;
    return $ fst $ fromJust $ M.lookup l store;
  }

askType :: Ident -> MRSIO Type
askType ident = do loc <- getLoc ident
                   getTypeFromStore loc

getStore :: MRSIO Store
getStore = lift $ get

printStore = do store <- getStore
                putStr_IO $ show store

alloc :: Type -> MRSIO Loc
alloc t = do {
    state <- getStore;
    let size = M.size state
      in do lift $ put (M.insert size (t, Null) state);
            return size
  }

putStore :: Loc -> EExp -> MRSIO ()
putStore loc v = do {
    state <- getStore;
    let t = fst $ fromJust $ M.lookup loc state;
      in lift $ put (M.insert loc (t, v) state);
    return ()
  }


convertVar converter ident = do v <- askExp ident
                                converter v

calcInt :: EExp -> MRSIO Integer
calcInt x = case x of
    EVar ident     -> convertVar calcInt ident
    EAdd exp1 exp2 -> do n1 <- calcInt exp1
                         n2 <- calcInt exp2
                         return $ n2 + n1
    ESub exp1 exp2 -> do n1 <- calcInt exp1
                         n2 <- calcInt exp2
                         return $ n2 - n1
    EMul exp1 exp2 -> do n1 <- calcInt exp1
                         n2 <- calcInt exp2
                         return $ n2 * n1
    EDiv exp1 exp2 -> do n1 <- calcInt exp1
                         n2 <- calcInt exp2
                         return $ n2 `div` n1
    EInt n         -> return n
    EKey cont k    -> getFromCont cont k
      where
        getFromCont (EArrII a) (EInt i1) = do i <- simint (EInt i1)
                                              return $ toInteger $ a ! i

        getFromCont (EVar ident) key     = do a <- askExp ident
                                              getFromCont a key

                                              --return $ toInteger $ a ! i


calcExpInt :: EExp -> MRSIO EExp
calcExpInt exp = do int <- calcInt exp
                    return $ EInt $ toInteger int


calcChar :: EExp -> MRSIO EExp
calcChar (EChar c) = return $ EChar c
calcChar (EVar i)  = convertVar calcChar i
calcChar (EKey container k) = getFromCont container k
  where
    getFromCont (EStr s) k       = do i <- simint k
                                      return $ EChar $ head $ drop i s
    getFromCont (EVar ident) key = do a <- askExp ident
                                      getFromCont a key

getContainerKeyType :: Type -> Type
getContainerKeyType TStr     = TInt
getContainerKeyType (TArr t1 t2)  = t1

getContainerValueType :: Type -> Type
getContainerValueType TStr   = TChar
getContainerValueType (TArr t1 t2)= t2


getType :: EExp -> MRSIO Type
getType exp = case exp of
    BTrue       -> return TBool
    BFalse      -> return TBool

    EOr _ _     -> return TBool
    EAnd _ _    -> return TBool
    EAss _ _    -> return TBool
    ENAss _ _   -> return TBool
    ELt _ _     -> return TBool
    EGt _ _     -> return TBool
    ELEt _ _    -> return TBool
    EGEt _ _    -> return TBool

    EAdd e _    -> getType e
    ESub _ _    -> return TInt
    EMul _ _    -> return TInt
    EDiv _ _    -> return TInt

    EInt _      -> return TInt
    EChar _     -> return TChar
    EStr _      -> return TStr
    EFSub _     -> return TStr
    ELSub _ _   -> return TStr
    ERSub _ _   -> return TStr
    ELRSub _ _ _-> return TStr
    -- TODO consider it
    EKey e _    -> do con_t <- getType e
                      return $ getContainerValueType con_t
    ELen _      -> return TFunc
    EOrd _      -> return TFunc
    EArrII _    -> return (TArr TInt TInt)
    -- TODO
    EVar v      -> do {
        l <- getLoc v;
        getTypeFromStore l;
      }

concatenation (EStr s1) (EStr s2) = EStr (s1 ++ s2)


simint :: EExp -> MRSIO Int
simint i = do int <- calcInt i
              return $ fromIntegral $ int

calcString :: EExp -> MRSIO EExp
calcString str = case str of
    EStr s              -> return $ EStr s
    EAdd s1 s2          -> do
                              str1 <- calcString s1
                              str2 <- calcString s2
                              return $ concatenation str1 str2

    EFSub s             -> return s

    ELSub (EStr s) l    -> do i <- simint l
                              return $ EStr $ drop i s
    ERSub (EStr s) l    -> do i <- simint l
                              return $ EStr $ take i s
    ELRSub (EStr s) l r -> do {
        left <- simint l;
        right <- simint r;
        return $ EStr ((take (right - left)) $ drop left s)
      }
    EVar i              -> convertVar calcString i


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

calcFunc :: EExp -> MRSIO EExp
calcFunc (ELen (EStr s)) = return $ EInt (toInteger $ length s)
calcFunc (EOrd (EChar c)) = return $ EInt (toInteger $ ord c)


calcArr :: EExp -> MRSIO EExp
calcArr = (\x -> return x)

getConverter :: Type -> (EExp -> MRSIO EExp)
getConverter t = case t of
    TBool      -> calcBool
    TInt       -> calcExpInt
    TChar      -> calcChar
    TStr       -> calcString
    TFunc      -> calcFunc
    TArr _ _   -> calcArr


showExp (EInt i)    = show i
showExp (EStr s)    = s
showExp (EChar s)   = [s]
showExp BTrue       = "verum"
showExp BFalse      = "falsum"


createArray :: EExp -> EExp -> Type -> EExp
createArray (EInt i1) (EInt i2) TInt = EArrII (array (fromInteger i1, fromInteger i2) [])

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
        putStore loc (createArray e1 e2 t);
        localEnv (M.insert i loc) (iDecl tail stm);
    else
        -- TODO handle error here
        return_IO
  }


iDecl [] stm = interpretStmts stm


calcExp :: EExp -> MRSIO EExp
calcExp e = do t <- getType e
               getConverter t e


update_container :: EExp -> EExp -> EExp -> EExp
update_container (EStr s) (EInt i) (EChar c) = EStr (upd_con s i c)
  where
    upd_con (_:t) 0 c = (c:t)
    upd_con (h:t) i c = (h:upd_con t (i - 1) c)

update_container (EArrII a) (EInt i1) (EInt i2) = EArrII (a // [(fromInteger i1, fromInteger i2)])


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
                                  putStore loc exp
                              else
                                  -- TODO err
                                  return_IO

iStmt (STSet ident key value) = do {
    cont_t <- askType ident;
    value_t <- getType value;
    key_t <- getType key;
    if value_t == getContainerValueType cont_t && key_t == getContainerKeyType cont_t then
      do loc <- getLoc ident
         cont <- askExp ident
         value_exp <- calcExp value
         key_exp <- calcExp key
         putStore loc (update_container cont key_exp value_exp)
    else
        -- TODO err
        return_IO
  }
-- interpretStmts :: [Stm] -> IO ()
interpretStmts :: [Stm] -> MRSIO ()
interpretStmts [] = return_IO
interpretStmts (h:t) = do {
    iStmt h;
    interpretStmts t;
  }


interpretProg prog = runStateT (runReaderT (interpret_ prog) M.empty) M.empty
  where
    interpret_ :: Program -> MRSIO ()
    interpret_ (Prog _ d i) = iDecl d i
