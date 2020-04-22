# Printing

## Quick reference of common pretty-printing functions

* `string,int,bool,char,float`: `F.pp_string, ..., F.pp_float`
* `int`: `Format.pp_print_int`
* `bool`: `Format.pp_print_bool`
* `char`: `Format.pp_print_char`
* `float`: `Format.pp_print_float`
* `pos`: `pp_pos`
* `state`: `pp_state`
* `context`: `pp_context`
* `MP.t` (homogenous map): `MP.pp <pp_type_a> <pp_type b>`
* `SP.t` (set): `SP.pp <pp_type_a>`
* `HM.t` (heterogenous map): `HM.pp`

## How to use

Printing of data values stored in heterogenous maps is done by giving a pretty-printing (pp) function to their associated identifier's information. If no pp function is given, the value will be displayed as `<opaque>`.

The pp function is given when the data identifier is declared, e.g.

```
let k = data ~pp:Format.pp_print_int "some_int"
```

The pp function for type t should have type

```
Format.formatter -> t -> unit
```
and should rely on the module `F`. In particular, `F.p` is a shorthand for `Format.fprintf`.

For instance, here is the pp function for positions (at the time of writing) :

```
type pos = int * string (*Special NFT for positions*)
let pp_pos fmt (_,s) = F.p fmt "⦗%s⦘" s
```

For parametric types, a pp function should be provided for every type parameter. For instance, maps are of type `('a,'b) MP.t` where `'a` is the type of keys and `'b` is the type of values. When declaring a data identifier for a `pos` to `string` map, use the function `MP.pp` which has type

```
(Format.formatter -> 'a -> unit) -> (Format.formatter -> 'b -> unit) ->
Format.formatter -> ('a, 'b) t -> unit
```
and provide it with the pp functions for types `pos` and `string`:

```
let k = data ~pp:(MP.pp pp_pos Format.pp_print_string) "some_map"
```


