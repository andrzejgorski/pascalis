module Interpreter where

import AbsPascalis
import PrintPascalis
import ErrM
import Data.Char

import qualified Data.Map as M
import Control.Monad.Reader
import Data.Maybe(fromMaybe, fromJust)
import Control.Monad.State


type Var = String
type Loc = Int
type Env = M.Map Ident Loc
type Store = M.Map Loc (Type, Exp)
type MRSIO a = ReaderT Env (StateT Store IO) a


return_IO :: MRSIO ()
return_IO = lift $ lift $ return ()

putStr_IO :: String -> MRSIO ()
putStr_IO s = lift $ lift $ putStr s

askEnv :: MRSIO Env
askEnv = ask

getLoc :: Ident -> MRSIO Loc
getLoc v = do env <- askEnv
              return $ fromJust $ M.lookup v env

localEnv :: (Env -> Env) -> (MRSIO () -> MRSIO ())
localEnv f s = local f s

getExpFromStore :: Loc -> MRSIO Exp
getExpFromStore l = do {
    store <- getStore;
    return $ snd $ fromJust $ M.lookup l store;
  }

getTypeFromStore :: Loc -> MRSIO Type
getTypeFromStore l = do {
    store <- getStore;
    return $ fst $ fromJust $ M.lookup l store;
  }

getStore :: MRSIO Store
getStore = lift $ get

alloc :: Type -> MRSIO Loc
alloc t = do {
    state <- getStore;
    let size = M.size state
      in do lift $ put (M.insert size (t, Null) state);
            return size
  }

putStore :: Loc -> Exp -> MRSIO ()
putStore loc v = do {
    state <- getStore;
    let t = fst $ fromJust $ M.lookup loc state;
      in lift $ put (M.insert loc (t, Null) state);
    return ()
  }


calcInt x = case x of
    EAdd exp0 exp  -> calcInt exp0 + calcInt exp
    ESub exp0 exp  -> calcInt exp0 - calcInt exp
    EMul exp0 exp  -> calcInt exp0 * calcInt exp
    EDiv exp0 exp  -> calcInt exp0 `div` calcInt exp
    EInt n  -> n


calcExpInt :: Exp -> MRSIO Exp
calcExpInt exp = return $ EInt $ calcInt exp

calcChar :: Exp -> MRSIO Exp
calcChar (EChar c) = return $ EChar c


getType :: Exp -> MRSIO Type
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
    EKey _ _    -> return TStr
    ELen _      -> return TFunc
    EOrd _      -> return TFunc
    -- TODO
    EVar v      -> do {
        l <- getLoc v;
        getTypeFromStore l;
      }

concatenation (EStr s1) (EStr s2) = EStr (s1 ++ s2)


simint :: Exp -> MRSIO Int
simint i = return $ fromIntegral $ calcInt i

calcString :: Exp -> MRSIO Exp
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
    EKey (EStr s) k     -> do i <- simint k
                              return $ EChar $ head $ drop i s


calcBool :: Exp -> MRSIO Exp
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

calcFunc :: Exp -> MRSIO Exp
calcFunc (ELen (EStr s)) = return $ EInt (toInteger $ length s)
calcFunc (EOrd (EChar c)) = return $ EInt (toInteger $ ord c)


getConverter :: Type -> (Exp -> MRSIO Exp)
getConverter t = case t of
    TBool      -> calcBool
    TInt       -> calcExpInt
    TChar      -> calcChar
    TStr       -> calcString
    TFunc      -> calcFunc


showExp (EInt i)    = show i
showExp (EStr s)    = s
showExp (EChar s)   = [s]
showExp BTrue       = "verum"
showExp BFalse      = "falsum"

iDecl :: [Decl] -> [Stm] -> MRSIO ()
iDecl ((DVar ind ty):tail) stm = do {
    loc <- alloc ty;
    localEnv (M.insert ind loc) (iDecl tail stm);
  }

iDecl [] stm = interpretStmts stm

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


-- module Interpreter where
-- 
-- 
-- -- runStateT :: StateT s m a -> s -> m (a, s)
-- -- evalStateT :: Monad m => StateT s m a -> s -> m a
-- -- execStateT :: Monad m => StateT s m a -> s -> m s
-- 
-- data Exp = EInt Int
--     | EOp  Op Exp Exp
--     | EVar Var
--     | ELet Var Exp Exp  -- let var = e1 in e2
-- 
-- data Op = OpAdd | OpMul | OpSub
-- 
-- 
-- eval :: Exp -> Reader (M.Map Var Int) Int
-- eval (EInt i) = return i
-- 
-- eval (EOp op e1 e2) = do
--   ce1 <- eval e1
--   ce2 <- eval e2
--   return $ calc op ce1 ce2
--     where
--       calc OpAdd e1 e2 = e1 + e2
--       calc OpMul e1 e2 = e1 * e2
--       calc OpSub e1 e2 = e1 - e2
-- 
-- eval (EVar var) = do
--     map <- ask
--     return $ fromMaybe 0 (M.lookup var map)
-- 
-- eval (ELet var e1 e2) = do
--     ce1 <- eval e1
--     local (M.insert var ce1) (eval e2)
-- 
-- evalExp :: Exp -> Int
-- evalExp e = runReader (eval e) M.empty
-- 
-- data Decl = DLet Var Exp
-- 
-- data Stmt = Skip
--     | SVar Var Exp
--     | SLet Var Exp
--     | SComp Stmt Stmt
--     | SIf Exp Stmt Stmt
--     | SWhile Exp Stmt
--     -- | SBlock Decl Stmt
-- 
-- -- type SS a = StateT Store (Reader Env) a
-- interpret :: Stmt -> SS ()
-- interpret Skip = return ()
-- 
-- 
-- interpret (SVar var exp) = do
--     state <- get
--     env <- ask
--     loc <- 0
-- 
--     local (M.insert var loc)
--     return ()
-- 
-- -- interpret (SLet var exp) = do
-- --     state <- get
-- --     put (M.insert var (runReader (eval exp) state) state)
-- --     return ()
-- 
-- 
-- -- interpret (SComp stmt1 stmt2) = do
-- --     interpret stmt1
-- --     interpret stmt2
-- --     return ()
-- -- 
-- -- interpret (SIf exp stmt1 stmt2) = do
-- --     state <- get
-- --     if ((runReader (eval exp) state) /= 0) then interpret stmt1 else interpret stmt2
-- --     return ()
-- -- 
-- -- interpret (SWhile exp stmt) = do
-- --     state <- get
-- --     if ((runReader (eval exp) state) /= 0) then do {interpret stmt; interpret (SWhile exp stmt); return ()} else return ()
-- 
-- 
-- -- execStmt :: Stmt -> IO ()
-- -- execStmt st = putStrLn $ show $ execStateT (interpret st) M.empty
-- 
-- -- stmtToStr :: Stmt -> String
-- -- stmtToStr st = show $ execStateT (interpret st) M.empty
-- stmtToStr st = runReader (execStateT (interpret st) M.empty) M.empty
-- 
-- 
-- -- type RR a = ReaderT Env (State Store) a
-- -- runReaderT :: ReaderT r m a -> r -> m a
-- --
-- -- type SS a = StateT Store (Reader Env) a
-- -- runStateT :: StateT s m a -> s -> m (a, s)
-- -- evalStateT :: Monad m => StateT s m a -> s -> m a
-- -- execStateT :: Monad m => StateT s m a -> s -> m s
-- --
-- -- lift: Monad m => a -> t m a
