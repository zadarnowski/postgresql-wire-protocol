build:
	@cabal new-build

doc:
	@cabal new-haddock

open-doc: doc
	@open -a Safari dist-newstyle/build/*/*/postgresql-wire-protocol-*/doc/html/postgresql-wire-protocol/index.html
