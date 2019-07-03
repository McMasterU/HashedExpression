format:
	hindent **/*.hs

check_exhaustive:
	stack clean
	stack build --fast --ghc-options -Wincomplete-patterns

