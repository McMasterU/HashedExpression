format:
	find src -type f -name '*.hs' | xargs hindent
	find test -type f -name '*.hs' | xargs hindent

check_exhaustive:
	stack clean
	stack build --fast --ghc-options -Wincomplete-patterns

clean:
	-git clean -f -x C/
	-rm *.dat
	-git clean -f -x plots/

parse:
	stack build --fast
	stack exec symphony example.he