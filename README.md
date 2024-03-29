# io-manager [![Hackage](https://img.shields.io/hackage/v/io-manager.svg?style=flat)](https://hackage.haskell.org/package/io-manager) [![Haskell Cabal](https://github.com/mihaimaruseac/io-manager/actions/workflows/presubmit-cabal.yaml/badge.svg)](https://github.com/mihaimaruseac/io-manager/actions/workflows/presubmit-cabal.yaml) [![Haskell Stack](https://github.com/mihaimaruseac/io-manager/actions/workflows/presubmit-stack.yaml/badge.svg)](https://github.com/mihaimaruseac/io-manager/actions/workflows/presubmit-stack.yaml) [![OpenSSF Scorecard](https://api.securityscorecards.dev/projects/github.com/mihaimaruseac/io-manager/badge)](https://api.securityscorecards.dev/projects/github.com/mihaimaruseac/io-manager)

This is a simple framework which hides away the IO monad and allows beginner
students to write simple applications in Haskell without having to touch the
`IO` monad for reading or writing to standard file descriptors or other files.

This skeleton comes with a simple example showing the usage of this system. As
you can see, only three lines are to be copied by the student:

* the `module Main` one
* the `import Training.MM.IOManager` one
* the `main = wrapIO ..` one

Everything else is free of advanced concepts like monads, mutability,
strictness, etc.

This skeleton can be used to set up a homework which can then be automatically
tested using tools like [vmchecker][vmchecker]

Example
=======

The repository contains an example code in `SimpleEchoExample.hs`: a
demonstrative program which copies content from files (or `stdin`) to files
(or `stdout`) depending on a description written at `stdin`.

Run `make` (or the usual `cabal` setup) to build it and then give as arguments
the name of the files from which you want to read. Finish `stdin` with `EOF`
(`^D` on Linux, `^Z` on Windows).

Example:

    ./SimpleEchoExample Makefile M.out unused
    Makefile out_Makefile
    M.out out_Mout
    @stdin out_stdin
    @stdin @stdout

Due to Haskell's laziness, `unused` will not be read.

[vmchecker]: https://github.com/vmchecker/vmchecker "vmchecker"
