format:
	find src -type f -name '*.hs' | xargs hindent
	find test -type f -name '*.hs' | xargs hindent

check:
	stack clean
	stack build --fast --ghc-options -Wall

clean:
	-git clean -f -x C/
	-rm *.dat
	-git clean -f -x plots/
