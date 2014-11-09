.PHONY: test setup run

setup:
	cabal install --enable-tests
	cabal configure --enable-tests

test:
	cabal build && cabal test
	cat dist/test/*-main.log

run:
	cabal build && cabal run
