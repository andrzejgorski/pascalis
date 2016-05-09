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

runBlock :: Env -> ([Stm] -> MRSIO ()) -> [Stm] -> MRSIO ()
runBlock env interpretFunc stms= do lift $ runReaderT (interpretFunc stms) env;
                                          return_IO

-- StateT monad
getStore :: MRSIO Store
getStore = lift $ get

putStore :: Store -> MRSIO ()
putStore s = lift $ put s

-- ReaderT monad
askEnv :: MRSIO Env
askEnv = ask

localEnv :: (Env -> Env) -> (MRSIO () -> MRSIO ())
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
              return $ fromJust $ M.lookup v env

askExp :: Ident -> MRSIO EExp
askExp ident = do loc <- getLoc ident
                  getExpFromStore loc

askType :: Ident -> MRSIO Type
askType ident = do loc <- getLoc ident
                   getTypeFromStore loc
