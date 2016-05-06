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
  prtList _ [] = (concatD [])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ","), prt 0 xs])
instance Print Stm where
  prt i e = case e of
    Skip -> prPrec i 0 (concatD [doc (showString "persulta"), doc (showString ";")])
    SPrint exp -> prPrec i 0 (concatD [doc (showString "incribo"), doc (showString "("), prt 0 exp, doc (showString ")"), doc (showString ";")])
    SIf bexp stm -> prPrec i 0 (concatD [doc (showString "si"), prt 0 bexp, doc (showString "tunc"), prt 0 stm])
    SIfElse bexp stm1 stm2 -> prPrec i 0 (concatD [doc (showString "si"), prt 0 bexp, doc (showString "tunc"), prt 0 stm1, doc (showString "alter"), prt 0 stm2])
    SDecl decl -> prPrec i 0 (concatD [prt 0 decl, doc (showString ";")])
    SExp exp -> prPrec i 0 (concatD [prt 0 exp, doc (showString ";")])
    SBlock stms -> prPrec i 0 (concatD [doc (showString "incipe"), prt 0 stms, doc (showString "fini")])
    SWhile exp stm -> prPrec i 0 (concatD [doc (showString "donec"), prt 0 exp, doc (showString "fac"), prt 0 stm])
    SReturn exp -> prPrec i 0 (concatD [doc (showString "refer"), prt 0 exp, doc (showString ";")])
  prtList _ [] = (concatD [])
  prtList _ (x:xs) = (concatD [prt 0 x, prt 0 xs])
instance Print BExp where
  prt i e = case e of
    BTrue -> prPrec i 0 (concatD [doc (showString "verum")])
    BFalse -> prPrec i 0 (concatD [doc (showString "falsum")])
    BOr bexp1 bexp2 -> prPrec i 0 (concatD [prt 0 bexp1, doc (showString "uel"), prt 0 bexp2])
    BAnd bexp1 bexp2 -> prPrec i 0 (concatD [prt 0 bexp1, doc (showString "et"), prt 0 bexp2])
    BAss bexp1 bexp2 -> prPrec i 0 (concatD [prt 0 bexp1, doc (showString "="), prt 0 bexp2])
    BNAss bexp1 bexp2 -> prPrec i 0 (concatD [prt 0 bexp1, doc (showString "<>"), prt 0 bexp2])
    EAss exp1 exp2 -> prPrec i 0 (concatD [prt 0 exp1, doc (showString "="), prt 0 exp2])
    ENAss exp1 exp2 -> prPrec i 0 (concatD [prt 0 exp1, doc (showString "<>"), prt 0 exp2])
    ELt exp1 exp2 -> prPrec i 0 (concatD [prt 2 exp1, doc (showString "<"), prt 2 exp2])
    EGt exp1 exp2 -> prPrec i 0 (concatD [prt 2 exp1, doc (showString ">"), prt 2 exp2])
    ELEt exp1 exp2 -> prPrec i 0 (concatD [prt 2 exp1, doc (showString "=<"), prt 2 exp2])
    EGEt exp1 exp2 -> prPrec i 0 (concatD [prt 2 exp1, doc (showString ">="), prt 2 exp2])
    CAss cexp1 cexp2 -> prPrec i 0 (concatD [prt 0 cexp1, doc (showString "="), prt 0 cexp2])
    CNAss cexp1 cexp2 -> prPrec i 0 (concatD [prt 0 cexp1, doc (showString "<>"), prt 0 cexp2])
    CLt cexp1 cexp2 -> prPrec i 0 (concatD [prt 0 cexp1, doc (showString "<"), prt 0 cexp2])
    CGt cexp1 cexp2 -> prPrec i 0 (concatD [prt 0 cexp1, doc (showString ">"), prt 0 cexp2])
    CLEt cexp1 cexp2 -> prPrec i 0 (concatD [prt 0 cexp1, doc (showString "=<"), prt 0 cexp2])
    CGEt cexp1 cexp2 -> prPrec i 0 (concatD [prt 0 cexp1, doc (showString ">="), prt 0 cexp2])

instance Print CExp where
  prt i e = case e of
    EChar c -> prPrec i 0 (concatD [prt 0 c])

instance Print Exp where
  prt i e = case e of
    EAdd exp1 exp2 -> prPrec i 2 (concatD [prt 2 exp1, doc (showString "+"), prt 3 exp2])
    ESub exp1 exp2 -> prPrec i 2 (concatD [prt 2 exp1, doc (showString "-"), prt 3 exp2])
    EMul exp1 exp2 -> prPrec i 3 (concatD [prt 3 exp1, doc (showString "*"), prt 4 exp2])
    EDiv exp1 exp2 -> prPrec i 3 (concatD [prt 3 exp1, doc (showString "/"), prt 4 exp2])
    Call id exps -> prPrec i 4 (concatD [prt 0 id, doc (showString "("), prt 0 exps, doc (showString ")")])
    EStr str -> prPrec i 4 (concatD [prt 0 str])
    EVar id -> prPrec i 4 (concatD [prt 0 id])
    EInt n -> prPrec i 4 (concatD [prt 0 n])
    EDouble d -> prPrec i 4 (concatD [prt 0 d])
  prtList _ [] = (concatD [])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ","), prt 0 xs])
instance Print Type where
  prt i e = case e of
    TInt -> prPrec i 0 (concatD [doc (showString "numeri integri")])


