Welcome to HDBC, Haskell Database Connectivity.

HDBC is modeled loosely on Perl's DBI interface, though it has also
been influenced by Python's DB-API v2, JDBC in Java, and HSQL in
Haskell.

Please see doc/Database-HDBC.html for an introduction to HDBC and its
various features.

INSTALLATION
------------

You'll need either GHC 6.4.1 or above, or Hugs 2005xx or above.

The steps to install are:

1) ghc --make -o setup Setup.lhs

2) ./setup configure

3) ./setup build

4) ./setup install   (as root)

If you're on Windows, you can omit the leading "./".

Documentation is in doc/ -- lots of information, including pointers to
drivers, is in doc/Database-HDBC.html.

USAGE
-----

To use with hugs, you'll want to use hugs -98.

To use with GHC, you'll want to use -package HDBC in your programs.
Or, with Cabal, use Build-Depends: HDBC.

-- John Goerzen
   December 2005
