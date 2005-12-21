all:
	@echo "Please use Cabal to build this package; not make."

.PHONY: doc
doc:
	-rm -r doc
	mkdir doc
	haddock -h -o doc `find Database -name "*.hs"`

