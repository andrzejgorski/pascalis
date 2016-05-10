-- This Happy file was machine-generated by the BNF converter
{
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module ParPascalis where
import AbsPascalis
import LexPascalis
import ErrM

}

%name pProgram Program
%name pDecl Decl
%name pListStm ListStm
%name pListDecl ListDecl
%name pListIdent ListIdent
%name pStm Stm
%name pExp Exp
%name pExp2 Exp2
%name pExp3 Exp3
%name pExp4 Exp4
%name pExp1 Exp1
%name pListExp ListExp
%name pType Type
-- no lexer declaration
%monad { Err } { thenM } { returnM }
%tokentype {Token}
%token
  '(' { PT _ (TS _ 1) }
  ')' { PT _ (TS _ 2) }
  '*' { PT _ (TS _ 3) }
  '+' { PT _ (TS _ 4) }
  ',' { PT _ (TS _ 5) }
  '-' { PT _ (TS _ 6) }
  '..' { PT _ (TS _ 7) }
  '/' { PT _ (TS _ 8) }
  ':' { PT _ (TS _ 9) }
  ':=' { PT _ (TS _ 10) }
  ':]' { PT _ (TS _ 11) }
  ';' { PT _ (TS _ 12) }
  '<' { PT _ (TS _ 13) }
  '<>' { PT _ (TS _ 14) }
  '=' { PT _ (TS _ 15) }
  '=<' { PT _ (TS _ 16) }
  '>' { PT _ (TS _ 17) }
  '>=' { PT _ (TS _ 18) }
  '[' { PT _ (TS _ 19) }
  '[:' { PT _ (TS _ 20) }
  '[:]' { PT _ (TS _ 21) }
  ']' { PT _ (TS _ 22) }
  'alter' { PT _ (TS _ 23) }
  'autem' { PT _ (TS _ 24) }
  'dictionarum' { PT _ (TS _ 25) }
  'donec' { PT _ (TS _ 26) }
  'et' { PT _ (TS _ 27) }
  'fac' { PT _ (TS _ 28) }
  'falsum' { PT _ (TS _ 29) }
  'fini' { PT _ (TS _ 30) }
  'fini.' { PT _ (TS _ 31) }
  'functio' { PT _ (TS _ 32) }
  'incipe' { PT _ (TS _ 33) }
  'litera' { PT _ (TS _ 34) }
  'logica booleana' { PT _ (TS _ 35) }
  'matrix' { PT _ (TS _ 36) }
  'non' { PT _ (TS _ 37) }
  'nullum' { PT _ (TS _ 38) }
  'numeri integri' { PT _ (TS _ 39) }
  'param' { PT _ (TS _ 40) }
  'persulta' { PT _ (TS _ 41) }
  'pro' { PT _ (TS _ 42) }
  'procedure' { PT _ (TS _ 43) }
  'processus' { PT _ (TS _ 44) }
  'program' { PT _ (TS _ 45) }
  'refer' { PT _ (TS _ 46) }
  'si' { PT _ (TS _ 47) }
  'titulus' { PT _ (TS _ 48) }
  'tunc' { PT _ (TS _ 49) }
  'uel' { PT _ (TS _ 50) }
  'ut' { PT _ (TS _ 51) }
  'variabilis' { PT _ (TS _ 52) }
  'verum' { PT _ (TS _ 53) }
  '{' { PT _ (TS _ 54) }
  '}' { PT _ (TS _ 55) }

L_ident  { PT _ (TV $$) }
L_quoted { PT _ (TL $$) }
L_charac { PT _ (TC $$) }
L_integ  { PT _ (TI $$) }
L_doubl  { PT _ (TD $$) }


%%

Ident   :: { Ident }   : L_ident  { Ident $1 }
String  :: { String }  : L_quoted {  $1 }
Char    :: { Char }    : L_charac { (read ( $1)) :: Char }
Integer :: { Integer } : L_integ  { (read ( $1)) :: Integer }
Double  :: { Double }  : L_doubl  { (read ( $1)) :: Double }

Program :: { Program }
Program : 'program' Ident ';' ListDecl 'incipe' ListStm 'fini.' { AbsPascalis.Prog $2 $4 (reverse $6) }
Decl :: { Decl }
Decl : 'variabilis' Ident ':' Type { AbsPascalis.DVar $2 $4 }
     | 'param' Ident ':' Type { AbsPascalis.DParam $2 $4 }
     | 'variabilis' Ident ':' 'matrix' '[' Exp '..' Exp ']' 'autem' Type { AbsPascalis.DAVar $2 $6 $8 $11 }
     | 'variabilis' Ident ':' 'matrix' '{' Type '}' 'autem' Type { AbsPascalis.DAPVar $2 $6 $9 }
     | 'processus' Ident '(' ListDecl ')' ';' ListDecl 'incipe' ListStm 'fini' { AbsPascalis.DProc $2 $4 $7 (reverse $9) }
     | 'functio' Ident '(' ListDecl ')' ':' Type ';' ListDecl 'incipe' ListStm 'fini' { AbsPascalis.DFunc $2 $4 $7 $9 (reverse $11) }
ListStm :: { [Stm] }
ListStm : {- empty -} { [] } | ListStm Stm { flip (:) $1 $2 }
ListDecl :: { [Decl] }
ListDecl : {- empty -} { [] }
         | Decl { (:[]) $1 }
         | Decl ';' ListDecl { (:) $1 $3 }
ListIdent :: { [Ident] }
ListIdent : Ident { (:[]) $1 } | Ident ',' ListIdent { (:) $1 $3 }
Stm :: { Stm }
Stm : 'persulta' ';' { AbsPascalis.Skip }
    | 'si' Exp 'tunc' Stm { AbsPascalis.SIf $2 $4 }
    | 'si' Exp 'tunc' Stm 'alter' Stm { AbsPascalis.SIfElse $2 $4 $6 }
    | Exp ';' { AbsPascalis.SExp $1 }
    | ListDecl { AbsPascalis.SDecl $1 }
    | 'incipe' ListStm 'fini' ';' { AbsPascalis.SBlock (reverse $2) }
    | 'refer' Exp ';' { AbsPascalis.SReturn $2 }
    | Ident '[' Exp ']' ':=' Exp ';' { AbsPascalis.STSet $1 $3 $6 }
    | Ident ':=' Exp ';' { AbsPascalis.SSet $1 $3 }
    | 'donec' Exp 'fac' Stm { AbsPascalis.SWhile $2 $4 }
    | 'pro' Ident '=' Exp 'ut' Exp 'fac' Stm { AbsPascalis.SFor $2 $4 $6 $8 }
Exp :: { Exp }
Exp : 'verum' { AbsPascalis.BTrue }
    | 'falsum' { AbsPascalis.BFalse }
    | 'nullum' { AbsPascalis.Null }
    | 'non' Exp { AbsPascalis.BNot $2 }
    | Exp4 '[:]' { AbsPascalis.EFSub $1 }
    | Exp4 '[' Exp ':]' { AbsPascalis.ELSub $1 $3 }
    | Exp4 '[:' Exp ']' { AbsPascalis.ERSub $1 $3 }
    | Exp4 '[' Exp ':' Exp ']' { AbsPascalis.ELRSub $1 $3 $5 }
    | Exp4 '[' Exp ']' { AbsPascalis.EKey $1 $3 }
    | Exp 'uel' Exp { AbsPascalis.EOr $1 $3 }
    | Exp 'et' Exp { AbsPascalis.EAnd $1 $3 }
    | Exp '=' Exp { AbsPascalis.EAss $1 $3 }
    | Exp '<>' Exp { AbsPascalis.ENAss $1 $3 }
    | Exp2 '<' Exp2 { AbsPascalis.ELt $1 $3 }
    | Exp2 '>' Exp2 { AbsPascalis.EGt $1 $3 }
    | Exp2 '=<' Exp2 { AbsPascalis.ELEt $1 $3 }
    | Exp2 '>=' Exp2 { AbsPascalis.EGEt $1 $3 }
    | Exp1 { $1 }
Exp2 :: { Exp }
Exp2 : Exp2 '+' Exp3 { AbsPascalis.EAdd $1 $3 }
     | Exp2 '-' Exp3 { AbsPascalis.ESub $1 $3 }
     | Exp3 { $1 }
Exp3 :: { Exp }
Exp3 : Exp3 '*' Exp4 { AbsPascalis.EMul $1 $3 }
     | Exp3 '/' Exp4 { AbsPascalis.EDiv $1 $3 }
     | Exp4 { $1 }
Exp4 :: { Exp }
Exp4 : Ident '(' ListExp ')' { AbsPascalis.Call $1 $3 }
     | String { AbsPascalis.EStr $1 }
     | Char { AbsPascalis.EChar $1 }
     | Ident { AbsPascalis.EVar $1 }
     | Integer { AbsPascalis.EInt $1 }
     | Double { AbsPascalis.EDouble $1 }
     | '(' Exp ')' { $2 }
Exp1 :: { Exp }
Exp1 : Exp2 { $1 }
ListExp :: { [Exp] }
ListExp : {- empty -} { [] }
        | Exp { (:[]) $1 }
        | Exp ',' ListExp { (:) $1 $3 }
Type :: { Type }
Type : 'numeri integri' { AbsPascalis.TInt }
     | 'logica booleana' { AbsPascalis.TBool }
     | 'titulus' { AbsPascalis.TStr }
     | 'litera' { AbsPascalis.TChar }
     | 'functio' { AbsPascalis.TFunc }
     | 'procedure' { AbsPascalis.TProc }
     | 'matrix' '{' Type '}' 'autem' Type { AbsPascalis.TArr $3 $6 }
     | 'dictionarum' '{' Type '}' 'autem' Type { AbsPascalis.TDict $3 $6 }
{

returnM :: a -> Err a
returnM = return

thenM :: Err a -> (a -> Err b) -> Err b
thenM = (>>=)

happyError :: [Token] -> Err a
happyError ts =
  Bad $ "syntax error at " ++ tokenPos ts ++ 
  case ts of
    [] -> []
    [Err _] -> " due to lexer error"
    _ -> " before " ++ unwords (map (id . prToken) (take 4 ts))

myLexer = tokens
}

