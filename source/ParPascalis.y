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
  '/' { PT _ (TS _ 7) }
  ':' { PT _ (TS _ 8) }
  ':]' { PT _ (TS _ 9) }
  ';' { PT _ (TS _ 10) }
  '<' { PT _ (TS _ 11) }
  '<>' { PT _ (TS _ 12) }
  '=' { PT _ (TS _ 13) }
  '=<' { PT _ (TS _ 14) }
  '>' { PT _ (TS _ 15) }
  '>=' { PT _ (TS _ 16) }
  '[' { PT _ (TS _ 17) }
  '[:' { PT _ (TS _ 18) }
  '[:]' { PT _ (TS _ 19) }
  ']' { PT _ (TS _ 20) }
  'alter' { PT _ (TS _ 21) }
  'donec' { PT _ (TS _ 22) }
  'et' { PT _ (TS _ 23) }
  'fac' { PT _ (TS _ 24) }
  'falsum' { PT _ (TS _ 25) }
  'fini' { PT _ (TS _ 26) }
  'fini.' { PT _ (TS _ 27) }
  'incipe' { PT _ (TS _ 28) }
  'incribo' { PT _ (TS _ 29) }
  'non' { PT _ (TS _ 30) }
  'numeri integri' { PT _ (TS _ 31) }
  'persulta' { PT _ (TS _ 32) }
  'program' { PT _ (TS _ 33) }
  'refer' { PT _ (TS _ 34) }
  'si' { PT _ (TS _ 35) }
  'tunc' { PT _ (TS _ 36) }
  'uel' { PT _ (TS _ 37) }
  'variabilis' { PT _ (TS _ 38) }
  'verum' { PT _ (TS _ 39) }

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
ListStm :: { [Stm] }
ListStm : {- empty -} { [] } | ListStm Stm { flip (:) $1 $2 }
ListDecl :: { [Decl] }
ListDecl : {- empty -} { [] }
         | Decl { (:[]) $1 }
         | Decl ',' ListDecl { (:) $1 $3 }
ListIdent :: { [Ident] }
ListIdent : Ident { (:[]) $1 } | Ident ',' ListIdent { (:) $1 $3 }
Stm :: { Stm }
Stm : 'persulta' ';' { AbsPascalis.Skip }
    | 'incribo' '(' Exp ')' ';' { AbsPascalis.SPrint $3 }
    | 'si' Exp 'tunc' Stm { AbsPascalis.SIf $2 $4 }
    | 'si' Exp 'tunc' Stm 'alter' Stm { AbsPascalis.SIfElse $2 $4 $6 }
    | Decl ';' { AbsPascalis.SDecl $1 }
    | Exp ';' { AbsPascalis.SExp $1 }
    | 'incipe' ListStm 'fini' { AbsPascalis.SBlock (reverse $2) }
    | 'donec' Exp 'fac' Stm { AbsPascalis.SWhile $2 $4 }
    | 'refer' Exp ';' { AbsPascalis.SReturn $2 }
Exp :: { Exp }
Exp : 'verum' { AbsPascalis.BTrue }
    | 'falsum' { AbsPascalis.BFalse }
    | 'non' Exp { AbsPascalis.BNot $2 }
    | Exp '[:]' { AbsPascalis.EFSub $1 }
    | Exp '[' Exp ':]' { AbsPascalis.ELSub $1 $3 }
    | Exp '[:' Exp ']' { AbsPascalis.ERSub $1 $3 }
    | Exp '[' Exp ':' Exp ']' { AbsPascalis.ELRSub $1 $3 $5 }
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

