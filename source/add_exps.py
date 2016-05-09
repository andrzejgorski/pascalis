#!/usr/bin/env python
import re

Exps = '''    | EArrII ArrII
'''

imports = '''import Data.Array
type ArrII = Array Int Int
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
