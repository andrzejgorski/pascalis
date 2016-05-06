module Interpreter where

import AbsPascalis
import PrintPascalis
import ErrM

-- interpret :: Exp -> Integer
calcInt x = case x of
    EAdd exp0 exp  -> calcInt exp0 + calcInt exp
    ESub exp0 exp  -> calcInt exp0 - calcInt exp
    EMul exp0 exp  -> calcInt exp0 * calcInt exp
    EDiv exp0 exp  -> calcInt exp0 `div` calcInt exp
    EInt n  -> n


calcBool exp = case exp of
    BTrue -> True
    BFalse -> False
    BAnd exp1 exp2 -> if calcBool exp1 then calcBool exp2 else False
    BOr exp1 exp2 -> if calcBool exp1 then True else calcBool exp2
    EBAss exp1 exp2 -> calcBool exp1 == calcBool exp2
    EBNAss exp1 exp2 -> calcBool exp1 /= calcBool exp2
    EAss exp1 exp2 -> calcInt exp1 == calcInt exp2
    ENAss exp1 exp2 -> calcInt exp1 /= calcInt exp2

    EAss exp1 exp2 -> calcInt exp1 == calcInt exp2
    ELt exp1 exp2 -> calcInt exp1 < calcInt exp2
    EGt exp1 exp2 -> calcInt exp1 > calcInt exp2

-- interpretStmts :: [Stm] -> IO ()
interpretStmts [] = return ()
interpretStmts (h:t) = do {
    iStmt h;
    interpretStmts t;
  }
  where
    iStmt Skip           = return ()
    iStmt (SPrint value) = putStr $ show $ calcInt value
    iStmt (SIf bexp stm) = if calcBool bexp then iStmt stm else return ()
    iStmt (SIfElse bexp stm1 stm2) = if calcBool bexp then iStmt stm1 else iStmt stm2

interpretProg (Prog _ _ stmts) = interpretStmts stmts




-- module Interpreter where
-- 
-- import qualified Data.Map as M
-- import Control.Monad.Reader
-- import Data.Maybe(fromMaybe)
-- import Control.Monad.State
-- 
-- 
-- type Var = String
-- type Loc = Int
-- type Env = M.Map Var Loc
-- type Store = M.Map Loc Int
-- type SS a = StateT Store (Reader Env) a
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
-- stmtToStr st = show $ runReader (execStateT (interpret st) M.empty) M.empty
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
