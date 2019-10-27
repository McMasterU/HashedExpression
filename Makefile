format:
	hindent src/*.hs
	hindent test/*.hs

check_exhaustive:
	stack clean
	stack build --fast --ghc-options -Wincomplete-patterns

clean:
	-git clean -f -x C/
	-rm *.dat
	-git clean -f -x plots/

parse:
	stack build --fast
	stack exec HashedExpression-lang example.he