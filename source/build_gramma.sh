#!/bin/bash

bnfc -m -haskell Pascalis.cf
mv TestPascalis.hs.bak TestPascalis.hs
mv Makefile.bak Makefile
python add_exps.py
