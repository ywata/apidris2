all:
	idris2 --build apidris2.ipkg
	build/exec/apidris2
	cabal build
