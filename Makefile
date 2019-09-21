format:
	hindent **/*.hs

check_exhaustive:
	stack clean
	stack build --fast --ghc-options -Wincomplete-patterns

clean:
	-git clean -f -x C/
	-rm *.dat
	-git clean -f -x plots/
