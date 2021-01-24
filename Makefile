all:
	idris2 --build apidris2.ipkg
	build/exec/apidris2
	cabal build

run : all
	./dist-newstyle/build/x86_64-osx/ghc-8.8.4/apidris2-0.1.0.0/x/apidris2-exe/build/apidris2-exe/apidris2-exe

try :
	./dist-newstyle/build/x86_64-osx/ghc-8.8.4/apidris2-0.1.0.0/x/apidris2-exe/build/apidris2-exe/apidris2-exe
