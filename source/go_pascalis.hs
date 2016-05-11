-- automatically generated by BNF Converter
module Main where


import System.IO ( stdin, hGetContents )
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure, exitSuccess )
import System.Process

import LexPascalis
import ParPascalis
import SkelPascalis
import PrintPascalis
import AbsPascalis
import Interpreter




import ErrM

type ParseFun a = [Token] -> Err a

myLLexer = myLexer

type Verbosity = Int

putStrV :: Verbosity -> String -> IO ()
putStrV v s = if v > 1 then putStrLn s else return ()

runFile v p f = readFile f >>= run v p

run v p s = let ts = myLLexer s in case p ts of
    Bad s -> do {
        putStrLn "\nParse              Failed...\n";
        putStrV v "Tokens:";
        putStrV v $ show ts;
        putStrLn s;
        exitFailure;
    }
    Ok  tree -> do {
        -- putStrLn "\nParse Successful!";
        interpretProg tree;
        exitSuccess;
    }


main :: IO ()
main = do
  args <- getArgs
  case args of
    fs -> mapM_ (runFile 0 pProgram) fs