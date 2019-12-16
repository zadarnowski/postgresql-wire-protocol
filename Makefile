build:
	@cabal new-build

clean:
	@rm -rf dist dist-newstyle

doc:
	@cabal new-haddock

open-doc: doc
	@open -a Safari dist-newstyle/build/*/*/postgresql-wire-protocol-*/doc/html/postgresql-wire-protocol/index.html
