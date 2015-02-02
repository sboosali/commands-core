PACKAGE = commands-core
VERSION = 0.0.0

HC = cabal exec -- ghc

CODE = sources # tests


# # # # # # # # # # # # # # # # # # 

default: run

all: test run document style

configure:
	cabal configure --enable-tests

build: configure
	cabal build

test: build
	cabal test
	@echo
	cat dist/test/*-tests.log

document:
	cabal configure
	cabal haddock
	open dist/doc/html/$(PACKAGE)/index.html

style:
	hlint --hint=HLint.hs  *.hs $(CODE)

check:
	cabal build --ghc-options="-fforce-recomp -fno-code"


# # # # # # # # # # # # # # # # # # 

default: Haskell

clean:
	rm -f Main *.{o,hi,dyn_o,dyn_hi}

fresh: clean
	rm -fr dist

.PHONY: default clean fresh all build test document style check

# # # # # # # # # # # # # # # # # # 
