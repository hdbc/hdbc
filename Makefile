all:
	@echo "Please use Cabal to build this package; not make."
	./Setup.lhs configure
	./Setup.lhs build

install:
	./Setup.lhs install

clean:
	./Setup.lhs clean

.PHONY: doc
doc:
	-rm -r doc
	mkdir doc
	haddock -h -t 'Haskell Database Connectivity (HDBC)' \
		-D doc/hdbc.interface \
		-o doc `find Database -name "*.hs"`

