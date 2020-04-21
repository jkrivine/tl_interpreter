# tl_interpreter

## Quickstart

### First, an overview of `lib/`:
* `lib/env.ml` contains the ethereum-style execution environment.
* `lib/address.ml` is an abstract address package.
* `lib/contracts/token.ml` is a normal contract which happens to implement a generic Token functionality.
* `lib/contracts/dec.ml` is a normal contract which happens to implement the Dec functionality.
* `tooling/` contains printing/maps/sets layers.

### To dive in
Start by reading/running the example files `bin/exampleN.ml` with e.g.
```
dune exec ./bin/exampleN.exe
```
then take a look at `bin/loan.ml`

### Adding files
Just add any `.ml` files to `bin/` and remember to add it to the list in `bin/dune`. Then, if you added the file `file.ml`, just run
```
dune exec ./bin/file.exe
```
Then to get an idea of what you can do :
* Read `lib/env.mli`
* Read `lib/contracts/dec.mli`
* Read `lib/contracts/dec.ml`
