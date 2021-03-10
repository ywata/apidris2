all: run


idr : typecheck
	idris2 --build apidris2.ipkg

conv: idr
	./build/exec/apidris2 test test/API.idr app/API.hs

typecheck :
	cd test; idris2 --typecheck test.ipkg

hs : conv
	cabal v2-build

APIDRIS2=./dist-newstyle/build/x86_64-osx/ghc-8.10.4/apidris2-0.0.0.0/x/apidris2-exe/build/apidris2-exe/apidris2-exe
run : hs
	${APIDRIS2} show

try :
	${APIDRIS2}

