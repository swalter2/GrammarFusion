all:
	happy -gca ParABNF.y
	alex -g LexABNF.x
	latex DocABNF.tex; dvips DocABNF.dvi -o DocABNF.ps
	ghc --make TestABNF.hs -o TestABNF
clean:
	-rm -f *.log *.aux *.hi *.o *.dvi
	-rm -f DocABNF.ps
distclean: clean
	-rm -f DocABNF.* LexABNF.* ParABNF.* LayoutABNF.* SkelABNF.* PrintABNF.* TestABNF.* AbsABNF.* TestABNF ErrM.* SharedString.* ABNF.dtd XMLABNF.* Makefile*

