OpenCL
======

A high-level OpenCL wrapper for Haskell.

Based on the [OpenCLRaw][] package by J.R. Heard.

  [OpenCLRaw]: http://hackage.haskell.org/package/OpenCLRaw

Installation
------------

**Requirements:** [c2hs][] must be installed. (Try `cabal install c2hs`.)

With the usual command to install as a user library:

    cabal install

Programs using the library must link against OpenCL; for example, by
passing `-lOpenCL` to GHC.

  [c2hs]: http://hackage.haskell.org/package/c2hs

Optional requirements
---------------------

Some OpenCL libraries require additional NUMA libraries. For instance,
on Ubuntu 11.04:

    sudo apt-get install libnuma1 libnuma-dev

Example
-------

There is an simple working example in the examples folder. You can create an
executable using:

    ghc --make -lOpenCL examples/example01.hs

Using GHCi
----------

It's possible to use GHCi with OpenCL, e.g.:

    ghci -lOpenCL examples/example01.hs
