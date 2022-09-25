.PHONY: run
run:
	cabal run learn-you-a-haskell

.PHONY: test
test:
	cabal run spec
