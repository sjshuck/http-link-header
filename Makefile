.PHONY: all build bench clean configure haddock hpc install repl test

all: install configure build haddock test hpc

build:
	cabal build

bench:
	rm benchmarks.tix || true
	cabal bench

clean:
	cabal clean
	if test -d .cabal-sandbox; then cabal sandbox delete; fi
	if test -d .hpc; then rm -r .hpc; fi

configure:
	cabal configure --enable-tests --enable-benchmarks --enable-coverage -v2

haddock:
	cabal haddock --hyperlink-source
	open dist/doc/html/http-link-header/index.html

hpc:
	hpc markup --destdir=tmp dist/hpc/tix/tests/tests.tix
	open tmp/hpc_index.html

install:
	cabal sandbox init
	cabal install --enable-tests --enable-benchmarks --only-dependencies --reorder-goals

repl:
	cabal repl lib:http-link-header --ghc-options="-fno-hpc"

test:
	cabal test tests --show-details=always --test-option=--color
