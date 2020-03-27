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

## When to use `let*`

### Quick rule
Blockchain commands are of type (simplified here): `'a st = state*context ->
value*state`. They take the global memory `state` of the chain and a calling
`context`; they return a `value` (or, in fact, an error) and an updated
`state`.

Use `let* x = a in b` whenever `a` depends in any way on the current `state` or
`context`; or just check the type of `a` if you have Merlin: if `a` is of type `something st`, then use `let*`. Otherwise use `let`.

### More details

The monadic operator `bind` chains commands together (simplified here):
```
bind : 'a st -> ('a -> 'b st) -> 'b st
bind c1 c2 = fun (state,context) -> 
  match c1 (state,context) with
  | (Ok v,state') -> c2 v (state',context)
  | (Error,_) -> (Error,state)
```

The infix version of `bind` is `>>=`; it is convenient for chaining commands:
```
(get_balance userA) >>= (fun balance -> <do something with userA's balance>)
```
where (for illustration purposes) `balance user (state,context)
= state.balances[user]`

`let*` is just additional syntactic sugar for `>>=` (which itself is `bind`). So :
`let* x = a in b` is the same as `a >>= fun x -> b` is the same as `bind a (fun x -> b)`.

The above example binds _before execution_ the result of
successfully computing `get_balance userA` to the variable `balance`. If
`balance userA` raises an `Error`, the definition of `>>=` will propagate the
error and ignore its second argument (`fun balance ...`).

This is similar to binding a value with the `let in` construct. Up to typing
and evaluation order, `let x = a in b` is the same as `(fun x -> b) a`. If
evaluating `a` raises an exception, the rest is not evaluated. 

If we define the infix operator `a (applyTo) f = f a`, we can write:

| native | monad |
| ---- | ----- |
| `` a `applyTo` (fun x -> b) `` | `a >>= (fun x -> b)` |
| `let x = a in b` | `let* x = a in b` |
