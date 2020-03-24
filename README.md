# tl_interpreter

## Quickstart

### First, an overview of `lib/`:
* `env.ml` contains the ethereum-style execution environment.
* `address.ml` is an abstract address package.
* `token.ml` is a normal contract which happens to implement a generic Token functionality.
* `dec.ml` is a normal contract which happens to implement the Dec functionality.
* `tooling/` contains printing/maps/sets layers.

### To dive in
Start by reading/running the example files in bin with e.g.

```
dune exec ./bin/exampleN.exe
```
then take a look at `loan.ml`

### Adding files
Just add any `.ml` files to `bin/` and remember to add it to the list in `bin/dune`. Then, if you added the file `file.ml`, just run

```
dune exec ./bin/file.exe
```

To get an idea of what you can do :
* Read `env.mli`
* Read `dec.mli`
* Read `dec.ml`
