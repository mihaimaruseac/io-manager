io-manager
==========

This is a simple framework which hides away the IO monad and allows beginner
students to write simple applications in Haskell, no matter how many files are
needed for input and output.

This skeleton comes with a simple example showing the usage of this system. As
you can see, only three lines are to be copied by the student:

* the `module Main` one
* the `import IOManager` one
* the `main = wrapIO ..` one

Everything else is free of advanced concepts like monads, mutability,
strictness, etc.

This skeleton can be used to set up a homework which can then be automatically
tested using tools like [vmchecker][vmchecker]

[vmchecker]: https://github.com/vmchecker/vmchecker "vmchecker"
