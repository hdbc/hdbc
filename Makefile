# Copyright (C) 2004 - 2009 John Goerzen <jgoerzen@complete.org>
#
#    This library is free software; you can redistribute it and/or
#    modify it under the terms of the GNU Lesser General Public
#    License as published by the Free Software Foundation; either
#    version 2.1 of the License, or (at your option) any later version.
#
#    This library is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
#    Lesser General Public License for more details.
#
#    You should have received a copy of the GNU Lesser General Public
#    License along with this library; if not, write to the Free Software
#    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
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
