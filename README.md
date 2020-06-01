# tl_interpreter

## Superquickstart
1. Read the [sandbox tutorial](https://raw.githubusercontent.com/jkrivine/tl_interpreter/master/doc/sandbox_tutorial_1.png)
2. ... together with the examples in bin/demo

## Quickstart

1. Read the [env tutorial](https://raw.githubusercontent.com/jkrivine/tl_interpreter/master/doc/env_tutorial_1.png)
2. Run example 0 (`dune exec ./bin/example0.exe`) and read the output & code.
3. Run example 1 (`dune exec ./bin/example1.exe`) and read the output & code.
4. Read the [dec tutorial](https://raw.githubusercontent.com/jkrivine/tl_interpreter/master/doc/dec_tutorial_1.png)
5. Run and read `loan.ml` (`dune exec ./bin/loan.exe`).

## Overview of `lib/`:
* `lib/env.ml` contains the ethereum-style execution environment.
* `lib/c.mli` and `lib/env.mli` contain most of the interface for the code in `lib/env.ml`, they are good files to read.
* `lib/address.ml` is an abstract address package.
* `lib/contracts/token.ml` is a normal contract which happens to implement a generic Token functionality.
* `lib/contracts/dec.ml` is a normal contract which happens to implement the Dec functionality.
* `lib/{imperative,monadic}.ml` provides a simple interface to both programming styles.
* `lib/macro.ml` contains example code for "z-crossing".
* `tooling/` contains printing/maps/sets layers.

## Adding files
Just add any `.ml` files to `bin/` and remember to add it to the list in `bin/dune`. Then, if you added the file `file.ml`, just run
```
dune exec ./bin/file.exe
```
Then to get an idea of what you can do :
* Read `lib/env.mli`
* Read `lib/contracts/dec.mli`
* Read `lib/contracts/dec.ml`
