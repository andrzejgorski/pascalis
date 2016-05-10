module MonadsFunctions where
import AbsPascalis

import qualified Data.Map as M
import Control.Monad.Reader
import Data.Maybe(fromJust)
import Control.Monad.State
import Data.Array

-- types
type MRSIO a = ReaderT Env (StateT Store IO) a


-- IO Monad
return_IO :: MRSIO ()
return_IO = lift $ lift $ return ()

putStr_IO :: String -> MRSIO ()
putStr_IO s = lift $ lift $ putStr s

getChar_IO :: MRSIO Char
getChar_IO = lift $ lift $ getChar

getLine_IO :: MRSIO String
getLine_IO = lift $ lift $ getLine

putStr_Err :: String -> MRSIO ()
putStr_Err s = lift $ lift $ putStr $ "Error: " ++ s ++ "\n"

runBlock :: Env -> ([Stm] -> MRSIO Exp) -> [Stm] -> MRSIO Exp
runBlock env interpretFunc stms = lift $ runReaderT (interpretFunc stms) env;

runNewEnv :: Env -> ([Decl] -> MRSIO (Env, Exp)) -> [Decl] -> MRSIO Env
runNewEnv env func decls = do env_exp <- lift $ runReaderT (func decls) env;
                              return $ fst env_exp


-- StateT monad
getStore :: MRSIO Store
getStore = lift $ get

putStore :: Store -> MRSIO ()
putStore s = lift $ put s

-- ReaderT monad
askEnv :: MRSIO Env
askEnv = ask

localEnv :: (Env -> Env) -> (MRSIO (Env, Exp) -> MRSIO (Env, Exp))
localEnv f s = local f s


-- help Monad functions

-- debug functions
printStore = do store <- getStore
                putStr_IO $ show store

printEnv = do env <- askEnv
              putStr_IO $ show env


-- help Store functions
alloc :: Type -> MRSIO Loc
alloc t = do {
    state <- getStore;
    let size = M.size state
      in do putStore (M.insert size (t, Null) state);
            return size
  }

putToStore :: Loc -> EExp -> MRSIO ()
putToStore loc v = do {
    state <- getStore;
    let t = fst $ fromJust $ M.lookup loc state;
      in putStore (M.insert loc (t, v) state);
    return ()
  }

getExpFromStore :: Loc -> MRSIO EExp
getExpFromStore l = do {
    store <- getStore;
    return $ snd $ fromJust $ M.lookup l store;
  }

getTypeFromStore :: Loc -> MRSIO Type
getTypeFromStore l = do {
    store <- getStore;
    return $ fst $ fromJust $ M.lookup l store;
  }


-- help Env functions
getLoc :: Ident -> MRSIO Loc
getLoc v = do env <- askEnv
              result <- return $ M.lookup v env
              case result of
                Just x  -> return x
                Nothing -> do putStr_Err $ "Cannot take from env " ++ show v ++ " location."
                              return (-1)

askExp :: Ident -> MRSIO EExp
askExp ident = do loc <- getLoc ident
                  getExpFromStore loc

askType :: Ident -> MRSIO Type
askType ident = do loc <- getLoc ident
                   getTypeFromStore loc
