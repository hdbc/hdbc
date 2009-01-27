all:
	@echo "Please use Cabal to build this package; not make."
	./Setup.lhs configure
	./Setup.lhs build

install:
	./Setup.lhs install

clean:
	./Setup.lhs clean

.PHONY: test
test: test-ghc test-hugs
	@echo ""
	@echo "All tests pass."

test-hugs:
	@echo " ****** Running hugs tests"
	./Setup.lhs configure --hugs
	./Setup.lhs build
	runhugs -98 +o -P$(PWD)/dist/scratch:$(PWD)/testsrc: testsrc/runtests.hs

test-ghc:
	@echo " ****** Building GHC tests"
	runghc Setup.lhs configure -f buildtests
	runghc Setup.lhs build
	@echo " ****** Running GHC tests"
	./dist/build/runtests/runtests
