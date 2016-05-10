#!/usr/bin/env python
import re

Exps = '''    | EArrI ArrI
    | EDict Dict
    | EOrd Exp
    | ELen Exp
    | EProc [Decl] [Stm] Env
    | EFunc [Decl] Type [Stm] Env
'''

imports = '''import Data.Array
import qualified Data.Map as M
type ArrI = Array Int Exp
type Dict = M.Map Exp Exp
type EExp = Exp
type Var = String
type Loc = Int
type Env = M.Map Ident Loc
type Store = M.Map Loc (Type, EExp)
'''
patternExp = '    | EInt Integer'
patternExp2 = ' | EInt Integer'
patternImports = 'module AbsPascalis where'

with open('AbsPascalis.hs', 'r') as f:
    data = f.read()

toInsert = {
    patternExp: Exps,
    patternExp2: Exps,
    patternImports: imports,
}


lines = data.split('\n')

result = []
for line in lines:
    result.append(line)
    for pattern in toInsert.keys():
        if pattern == line:
            result.append(toInsert[pattern])


with open('AbsPascalis.hs', 'w') as f:
    data = f.write('\n'.join(result))


print ('Add exps works fine! :)')
