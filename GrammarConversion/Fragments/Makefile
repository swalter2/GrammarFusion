all:
	happy -gca ParFragments.y
	alex -g LexFragments.x
	latex DocFragments.tex; dvips DocFragments.dvi -o DocFragments.ps
	ghc --make TestFragments.hs -o TestFragments
clean:
	-rm -f *.log *.aux *.hi *.o *.dvi
	-rm -f DocFragments.ps
distclean: clean
	-rm -f DocFragments.* LexFragments.* ParFragments.* LayoutFragments.* SkelFragments.* PrintFragments.* TestFragments.* AbsFragments.* TestFragments ErrM.* SharedString.* Fragments.dtd XMLFragments.* Makefile*

