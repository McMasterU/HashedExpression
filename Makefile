
FORMAT=ormolu --mode inplace -o -XTypeApplications -o -XTemplateHaskellQuotes

doc:
	stack haddock --haddock-arguments "--odir=docs/"

format:
	find src -type f -name '*.hs' | xargs $(FORMAT)
	find test -type f -name '*.hs' | xargs $(FORMAT)

clean:
	-git clean -f -x C/
	-rm *.dat
	-git clean -f -x plots/
