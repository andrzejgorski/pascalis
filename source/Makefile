all:
	happy -gca ParPascalis.y
	alex -g LexPascalis.x
	ghc --make TestPascalis.hs -o TestPascalis

clean:
	-rm -f *.log *.aux *.hi *.o *.dvi

gramma:
	./build_gramma.sh

distclean: clean
	-rm -f DocPascalis.* LexPascalis.* ParPascalis.* LayoutPascalis.* SkelPascalis.* PrintPascalis.* TestPascalis.* AbsPascalis.* TestPascalis ErrM.* SharedString.* ComposOp.* Pascalis.dtd XMLPascalis.* Makefile*


