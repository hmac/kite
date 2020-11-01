build:
	cabal build -O0

test:
	cabal test -O0 --test-show-details direct

.PHONY: build test
