
FORMAT=ormolu --mode inplace -o -XTypeApplications -o -XTemplateHaskellQuotes

format:
	find src -type f -name '*.hs' | xargs $(FORMAT)
	find test -type f -name '*.hs' | xargs $(FORMAT)
	find embed -type f -name '*.hs' | xargs $(FORMAT)


check:
	stack clean
	stack build --fast --ghc-options -Wall

clean:
	-git clean -f -x C/
	-rm *.dat
	-git clean -f -x plots/
