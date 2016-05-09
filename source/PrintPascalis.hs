{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module PrintPascalis where

-- pretty-printer generated by the BNF converter

import AbsPascalis
import Data.Char


-- the top-level printing method
printTree :: Print a => a -> String
printTree = render . prt 0

type Doc = [ShowS] -> [ShowS]

doc :: ShowS -> Doc
doc = (:)

render :: Doc -> String
render d = rend 0 (map ($ "") $ d []) "" where
  rend i ss = case ss of
    "["      :ts -> showChar '[' . rend i ts
    "("      :ts -> showChar '(' . rend i ts
    "{"      :ts -> showChar '{' . new (i+1) . rend (i+1) ts
    "}" : ";":ts -> new (i-1) . space "}" . showChar ';' . new (i-1) . rend (i-1) ts
    "}"      :ts -> new (i-1) . showChar '}' . new (i-1) . rend (i-1) ts
    ";"      :ts -> showChar ';' . new i . rend i ts
    t  : "," :ts -> showString t . space "," . rend i ts
    t  : ")" :ts -> showString t . showChar ')' . rend i ts
    t  : "]" :ts -> showString t . showChar ']' . rend i ts
    t        :ts -> space t . rend i ts
    _            -> id
  new i   = showChar '\n' . replicateS (2*i) (showChar ' ') . dropWhile isSpace
  space t = showString t . (\s -> if null s then "" else (' ':s))

parenth :: Doc -> Doc
parenth ss = doc (showChar '(') . ss . doc (showChar ')')

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

concatD :: [Doc] -> Doc
concatD = foldr (.) id

replicateS :: Int -> ShowS -> ShowS
replicateS n f = concatS (replicate n f)

-- the printer class does the job
class Print a where
  prt :: Int -> a -> Doc
  prtList :: Int -> [a] -> Doc
  prtList i = concatD . map (prt i)

instance Print a => Print [a] where
  prt = prtList

instance Print Char where
  prt _ s = doc (showChar '\'' . mkEsc '\'' s . showChar '\'')
  prtList _ s = doc (showChar '"' . concatS (map (mkEsc '"') s) . showChar '"')

mkEsc :: Char -> Char -> ShowS
mkEsc q s = case s of
  _ | s == q -> showChar '\\' . showChar s
  '\\'-> showString "\\\\"
  '\n' -> showString "\\n"
  '\t' -> showString "\\t"
  _ -> showChar s

prPrec :: Int -> Int -> Doc -> Doc
prPrec i j = if j<i then parenth else id


instance Print Integer where
  prt _ x = doc (shows x)


instance Print Double where
  prt _ x = doc (shows x)


instance Print Ident where
  prt _ (Ident i) = doc (showString ( i))
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ","), prt 0 xs])


instance Print Program where
  prt i e = case e of
    Prog id decls stms -> prPrec i 0 (concatD [doc (showString "program"), prt 0 id, doc (showString ";"), prt 0 decls, doc (showString "incipe"), prt 0 stms, doc (showString "fini.")])

instance Print Decl where
  prt i e = case e of
    DVar id type_ -> prPrec i 0 (concatD [doc (showString "variabilis"), prt 0 id, doc (showString ":"), prt 0 type_])
    DAVar id exp1 exp2 type_ -> prPrec i 0 (concatD [doc (showString "variabilis"), prt 0 id, doc (showString ":"), doc (showString "matrix"), doc (showString "["), prt 0 exp1, doc (showString ".."), prt 0 exp2, doc (showString "]"), doc (showString "autem"), prt 0 type_])
    DProc id decls1 decls2 stms -> prPrec i 0 (concatD [doc (showString "processus"), prt 0 id, doc (showString "("), prt 0 decls1, doc (showString ")"), doc (showString ";"), prt 0 decls2, doc (showString "incipe"), prt 0 stms, doc (showString "fini")])
  prtList _ [] = (concatD [])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ";"), prt 0 xs])
instance Print Stm where
  prt i e = case e of
    Skip -> prPrec i 0 (concatD [doc (showString "persulta"), doc (showString ";")])
    SPrint exp -> prPrec i 0 (concatD [doc (showString "incribo"), doc (showString "("), prt 0 exp, doc (showString ")"), doc (showString ";")])
    SIf exp stm -> prPrec i 0 (concatD [doc (showString "si"), prt 0 exp, doc (showString "tunc"), prt 0 stm])
    SIfElse exp stm1 stm2 -> prPrec i 0 (concatD [doc (showString "si"), prt 0 exp, doc (showString "tunc"), prt 0 stm1, doc (showString "alter"), prt 0 stm2])
    SExp exp -> prPrec i 0 (concatD [prt 0 exp, doc (showString ";")])
    SDecl decls -> prPrec i 0 (concatD [prt 0 decls])
    SBlock stms -> prPrec i 0 (concatD [doc (showString "incipe"), prt 0 stms, doc (showString "fini"), doc (showString ";")])
    SReturn exp -> prPrec i 0 (concatD [doc (showString "refer"), prt 0 exp, doc (showString ";")])
    STSet id exp1 exp2 -> prPrec i 0 (concatD [prt 0 id, doc (showString "["), prt 0 exp1, doc (showString "]"), doc (showString ":="), prt 0 exp2, doc (showString ";")])
    SSet id exp -> prPrec i 0 (concatD [prt 0 id, doc (showString ":="), prt 0 exp, doc (showString ";")])
    SWhile exp stm -> prPrec i 0 (concatD [doc (showString "donec"), prt 0 exp, doc (showString "fac"), prt 0 stm])
    SFor id exp1 exp2 stm -> prPrec i 0 (concatD [doc (showString "pro"), prt 0 id, doc (showString "="), prt 0 exp1, doc (showString "ut"), prt 0 exp2, doc (showString "fac"), prt 0 stm])
  prtList _ [] = (concatD [])
  prtList _ (x:xs) = (concatD [prt 0 x, prt 0 xs])
instance Print Exp where
  prt i e = case e of
    BTrue -> prPrec i 0 (concatD [doc (showString "verum")])
    BFalse -> prPrec i 0 (concatD [doc (showString "falsum")])
    Null -> prPrec i 0 (concatD [doc (showString "nullum")])
    BNot exp -> prPrec i 0 (concatD [doc (showString "non"), prt 0 exp])
    EFSub exp -> prPrec i 0 (concatD [prt 0 exp, doc (showString "[:]")])
    ELSub exp1 exp2 -> prPrec i 0 (concatD [prt 0 exp1, doc (showString "["), prt 0 exp2, doc (showString ":]")])
    ERSub exp1 exp2 -> prPrec i 0 (concatD [prt 0 exp1, doc (showString "[:"), prt 0 exp2, doc (showString "]")])
    ELRSub exp1 exp2 exp3 -> prPrec i 0 (concatD [prt 0 exp1, doc (showString "["), prt 0 exp2, doc (showString ":"), prt 0 exp3, doc (showString "]")])
    EKey exp1 exp2 -> prPrec i 0 (concatD [prt 0 exp1, doc (showString "["), prt 0 exp2, doc (showString "]")])
    ELen exp -> prPrec i 0 (concatD [doc (showString "longitudo"), doc (showString "("), prt 0 exp, doc (showString ")")])
    EOrd exp -> prPrec i 0 (concatD [doc (showString "ord"), doc (showString "("), prt 0 exp, doc (showString ")")])
    EOr exp1 exp2 -> prPrec i 0 (concatD [prt 0 exp1, doc (showString "uel"), prt 0 exp2])
    EAnd exp1 exp2 -> prPrec i 0 (concatD [prt 0 exp1, doc (showString "et"), prt 0 exp2])
    EAss exp1 exp2 -> prPrec i 0 (concatD [prt 0 exp1, doc (showString "="), prt 0 exp2])
    ENAss exp1 exp2 -> prPrec i 0 (concatD [prt 0 exp1, doc (showString "<>"), prt 0 exp2])
    ELt exp1 exp2 -> prPrec i 0 (concatD [prt 2 exp1, doc (showString "<"), prt 2 exp2])
    EGt exp1 exp2 -> prPrec i 0 (concatD [prt 2 exp1, doc (showString ">"), prt 2 exp2])
    ELEt exp1 exp2 -> prPrec i 0 (concatD [prt 2 exp1, doc (showString "=<"), prt 2 exp2])
    EGEt exp1 exp2 -> prPrec i 0 (concatD [prt 2 exp1, doc (showString ">="), prt 2 exp2])
    EAdd exp1 exp2 -> prPrec i 2 (concatD [prt 2 exp1, doc (showString "+"), prt 3 exp2])
    ESub exp1 exp2 -> prPrec i 2 (concatD [prt 2 exp1, doc (showString "-"), prt 3 exp2])
    EMul exp1 exp2 -> prPrec i 3 (concatD [prt 3 exp1, doc (showString "*"), prt 4 exp2])
    EDiv exp1 exp2 -> prPrec i 3 (concatD [prt 3 exp1, doc (showString "/"), prt 4 exp2])
    Call id exps -> prPrec i 4 (concatD [prt 0 id, doc (showString "("), prt 0 exps, doc (showString ")")])
    EStr str -> prPrec i 4 (concatD [prt 0 str])
    EChar c -> prPrec i 4 (concatD [prt 0 c])
    EVar id -> prPrec i 4 (concatD [prt 0 id])
    EInt n -> prPrec i 4 (concatD [prt 0 n])
    EDouble d -> prPrec i 4 (concatD [prt 0 d])
  prtList _ [] = (concatD [])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ","), prt 0 xs])
instance Print Type where
  prt i e = case e of
    TInt -> prPrec i 0 (concatD [doc (showString "numeri integri")])
    TBool -> prPrec i 0 (concatD [doc (showString "logica booleana")])
    TStr -> prPrec i 0 (concatD [doc (showString "titulus")])
    TChar -> prPrec i 0 (concatD [doc (showString "litera")])
    TFunc -> prPrec i 0 (concatD [doc (showString "functio")])
    TProc -> prPrec i 0 (concatD [doc (showString "procedure")])
    TArr type_1 type_2 -> prPrec i 0 (concatD [doc (showString "matrix"), doc (showString "{"), prt 0 type_1, doc (showString "}"), doc (showString "autem"), prt 0 type_2])
    TDict type_1 type_2 -> prPrec i 0 (concatD [doc (showString "dictionarum"), doc (showString "{"), prt 0 type_1, doc (showString "}"), doc (showString "autem"), prt 0 type_2])


