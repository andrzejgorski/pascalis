#!/usr/bin/env python
import re

Exps = '''    | EArrII ArrII
    | EProc [Decl] [Stm] Env
'''

imports = '''import Data.Array
import qualified Data.Map as M
type ArrII = Array Int Int
type EExp = Exp
type Var = String
type Loc = Int
type Env = M.Map Ident Loc
type Store = M.Map Loc (Type, EExp)
'''
patternExp = '    | EInt Integer'
patternImports = 'module AbsPascalis where'

with open('AbsPascalis.hs', 'r') as f:
    data = f.read()

toInsert = {
    patternExp: Exps,
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
