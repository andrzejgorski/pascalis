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
type Env = M.Map Var Loc
type Store = M.Map Loc (Type, Exp)
type MRSIO a = ReaderT Env (StateT Store IO) a


return_IO :: MRSIO ()
return_IO = lift $ lift $ return ()

putStr_IO :: String -> MRSIO ()
putStr_IO s = lift $ lift $ putStr s

askEnv :: MRSIO Env
askEnv = ask

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


calcExpInt exp = EInt $ calcInt exp


calcChar (EChar c) = (EChar c)


getType exp = case exp of
    BTrue       -> TBool
    BFalse      -> TBool

    EOr _ _     -> TBool
    EAnd _ _    -> TBool
    EAss _ _    -> TBool
    ENAss _ _   -> TBool
    ELt _ _     -> TBool
    EGt _ _     -> TBool
    ELEt _ _    -> TBool
    EGEt _ _    -> TBool

    EAdd e _    -> getType e
    ESub _ _    -> TInt
    EMul _ _    -> TInt
    EDiv _ _    -> TInt

    EInt _      -> TInt
    EChar _     -> TChar
    EStr _      -> TStr
    EFSub _     -> TStr
    ELSub _ _   -> TStr
    ERSub _ _   -> TStr
    ELRSub _ _ _-> TStr
    EKey _ _    -> TStr
    ELen _      -> TFunc
    EOrd _      -> TFunc
    -- TODO
    -- EVar     ->


concatenation (EStr s1) (EStr s2) = EStr (s1 ++ s2)


simint i = fromIntegral $ calcInt i
calcString str = case str of
    EStr s              -> EStr s
    EAdd s1 s2          -> concatenation (calcString s1) (calcString s2)
    EFSub s             -> s
    ELSub (EStr s) l    -> EStr (drop (simint l) s)
    ERSub (EStr s) r    -> EStr (take (simint r) s)
    ELRSub (EStr s) l r -> let {
        left = simint l;
        right = simint r - left;
    } in EStr ((take right) $ drop left s)
    EKey (EStr s) l     -> EChar $ head $ drop (simint l) s


calcBool exp = case exp of

    -- bool expressions
    BTrue           -> BTrue
    BFalse          -> BFalse
    BNot v          -> if calcBool v == BTrue then
                            BFalse
                       else
                            BTrue

    EAnd exp1 exp2  -> if calcBool exp1 == BTrue then
                            calcBool exp2
                       else
                            BFalse

    EOr exp1 exp2   -> if calcBool exp1 == BTrue then
                            BTrue
                       else
                            calcBool exp2

    EAss exp1 exp2  -> calcConvered (==) exp1 exp2
    ENAss exp1 exp2 -> calcConvered (/=) exp1 exp2
    ELt exp1 exp2   -> calcConvered (<)  exp1 exp2
    EGt exp1 exp2   -> calcConvered (>)  exp1 exp2
    ELEt exp1 exp2  -> calcConvered (<=) exp1 exp2
    EGEt exp1 exp2  -> calcConvered (>=) exp1 exp2
  where
    calcConvered func exp1 exp2 = let {
            t1 = getType exp1;
            t2 = getType exp2;
            converter = getConverter t1;
        }
        in if t1 == t2 then
            if func (converter exp1) (converter exp2) then
                BTrue
            else
                BFalse
        else
            -- TODO handle errors
            BFalse


calcFunc (ELen (EStr s)) = EInt (toInteger $ length s)
calcFunc (EOrd (EChar c)) = EInt (toInteger $ ord c)


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

iDecl :: Decl -> MRSIO ()
iDecl (DVar ind t) = return_IO
--do {
--        store <- getStore;
--    }


iStmt :: Stm -> MRSIO ()
iStmt Skip           = return_IO
iStmt (SPrint value) = putStr_IO $ showExp $ getConverter (getType value) value
iStmt (SExp value)   = interSExp value
  where
    interSExp (Call (Ident "incribe") list) = printParams list
      where
        printParams [] = return ()
        printParams (h:t) = do{iStmt (SPrint h); printParams t}

iStmt (SIf exp stm)  = if calcBool exp == BTrue then iStmt stm else return ()


iStmt (SIfElse exp stm1 stm2) = if calcBool exp == BTrue then iStmt stm1 else iStmt stm2



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
    interpret_ (Prog _ d i) = do {
        interpretStmts i;
    }


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
