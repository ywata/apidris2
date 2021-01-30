all: idr hs try


idr :
	idris2 --build apidris2.ipkg

conv:
	./build/exec/apidris2 spec/API.idr
hs : 
	cabal build

run : hs
	./dist-newstyle/build/x86_64-osx/ghc-8.8.4/apidris2-0.1.0.0/x/apidris2-exe/build/apidris2-exe/apidris2-exe

try :
	./dist-newstyle/build/x86_64-osx/ghc-8.8.4/apidris2-0.1.0.0/x/apidris2-exe/build/apidris2-exe/apidris2-exe
