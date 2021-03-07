all: run


idr : typecheck
	idris2 --build apidris2.ipkg

conv: idr
	./build/exec/apidris2 spec/API.idr app/API.hs

typecheck :
	cd spec; idris2 --typecheck spec.ipkg

hs : conv
	cabal v2-build

APIDRIS2=./dist-newstyle/build/x86_64-osx/ghc-8.10.4/apidris2-0.0.0.0/x/apidris2-exe/build/apidris2-exe/apidris2-exe
run : hs
	${APIDRIS2}

try :
	${APIDRIS2}

