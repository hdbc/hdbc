all: setup
	@echo "Please use Cabal to build this package; not make."
	./setup configure
	./setup build

setup: Setup.lhs
	ghc --make -o setup Setup.lhs

install: setup
	./setup install

clean:
	./Setup.lhs clean

.PHONY: test
test: test-ghc test-hugs
	@echo ""
	@echo "All tests pass."

test-hugs: setup
	@echo " ****** Running hugs tests"
	./setup configure -f buildtests --hugs
	./setup build
	runhugs -98 +o -P$(PWD)/dist/scratch:$(PWD)/dist/scratch/programs/runtests: \
		dist/scratch/programs/runtests/Main.hs

test-ghc: setup
	@echo " ****** Building GHC tests"
	./setup configure -f buildtests
	./setup build
	@echo " ****** Running GHC tests"
	./dist/build/runtests/runtests
