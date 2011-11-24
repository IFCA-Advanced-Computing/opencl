* Introduction
OpenCL highlevel wrapper for Haskell.

Based on Jeff Heard OpenCLRaw.

* Install the package

  Requisite: c2hs installed.

  With the usual commands to install as a user library:
  
  : runhaskell Setup configure --user
  : runhaskell Setup build
  : runhaskell Setup install

  The programs that use the library, need to link against OpenCL

** Optional Requisites
   Some OpenCL libraries required also numa libs. E.g, on Ubuntu 11.04:
   
   : install libnuma1 libnuma-dev

** Example
   
   There is an simple working example in the examples folder. You can create an
   executable using:

   : ghc --make -lOpenCL examples/example01.hs

** Executing on ghci

   It's possible to execute the command line interface of ghc linking with
   OpenCL, e.g:

   : ghci -lOpenCL examples/example01.hs

