# 7/5/20
## Why is box pointing to to the left segment?
The reason why I put the 'segment' link on the box of the target pos (instead of the target pos itself) is I think to have a way of knowing whether an address is part of a tradeline without an additional map/set. When I did this, singleton pos+box did not exist (you created a tl of length 1 immediately, and pos+box as dismantled after the last reduce/commit). So the box of the head of a tl had no mapping at all, so you didn't know if it was part of the tl or not. A 'source' (or 'origin') mapping would have solved the issue. I decided to let boxes point to the left segment, that way the head always points to the something.

Currently I have decided to init tls with a singleton pos+box. In that case the box has no mapping at all.

I could
* have a special "initial" segment. But anyone else could point to it...
* go back to the previous version
* use the 'origin' mapping (currently doing this)
* add a 'pos' mapping, which is just a box->pos backward arrow

## master_of, owner_of
The problem is: if segment `s` between `p` and `p'` receives an order to reduce and wants to validate that the caller is authorized, how does `s` do it?

`caller = owner_of p` is not sufficient because `p` may be owned by position `q`. positions are not contracts so they cannot call (positions are not contract because we don't want to pay for contract creation all the time. We could have a 'contract pool' that gets reused? Sounds horrible). 

So we introduce an additional concept, `master_of`.

If `a` is a pos in a tl with a segment, `master_of a = segment_of a`
If `a` is a pos in a tl without a segment but an owner, `master_of a = master_of (owner_of a)`
Otherwise, `master_of a = a`. This applies to users and regular contracts. If `a` is a pos, has no segment and has no owner, there's a bug in Dec.

Now, all `s` has to do is check `caller = master_of (owner_of p)`

Note that this is a bit hackish and I'm still looking for a cleaner architecture. Ideally :
* `owner_of` never throws
* ` master_of` never throws
* there are no edge cases such as the pos of an iniitial pos+box, or the box of a final pos+box. In the latter case, master_of returns the box itself, instead of the user!

# 3/5/20

Idea for new internal structure. "position" is a pair of _legs_ (formerly box). Both legs are owner by someone (note that means legs can be separated). There is a wrapper function `transfer_position` which always transfers both legs at the same time.

Why? Because currently I have the following issues : suppose a pos becomes a singleton. The attached box cannot be collected immediately because unlike with a pulled/committed pos, there is no automatic operation to detach the box and give it an owner. So I need a `free_singleton` command.
But now it means I need an additional call whenever a position happens to be the last. And if I test for singleton and free the pos automatically, it means that there is a discrepancy :
if `u --- v --- w` and v pulls, I get to `u --- v`, and v can regrow
if `v --- w` and v pulls, now v is singleton. If I free it, it cannot regrow. This is not uniform behavior.

One way to make it all uniform is to _partially free_, by saying ok, if
`u --- v --- w` and v pulls, I get to `u --- v` and now _v.box is freed_, because it isn't slave to any segment.
Similarly at any point in time the origin if a tradeline has a "free" position in the sense that its possessions are collectable.

Now automatically the case `v --- w`, v pulls, v means that v is free (it already was) and v's box becomes free (coherent with the general case where someone is on the left of v).

The only way I see how to implement the above is by having 2 legs and direct ownership. The `collectable pos` is true iff pos is leftmost, and `collectable box` is true iff box is rightmost.


# 20/4/20
For the imperative/monadic versions, it *is* possible to have them all be exactly the same implementation.

You need an interface which can translate to/from the reference (which is monadic):

```
module type Interface = sig
 type 'a st
 type 'a unit_st
 val wrap : 'a Nucleus.st -> 'a st
 val unwrap : ('a -> 'b st) -> 'a -> 'b Nucleus.st
end
```

And a functor which contains all the common operations of, e.g. `Program` :

```
module InnerProgram(M:Interface) = struct
 type 'a = 'a M.st
 type 'a unit_st = 'a M.unit_st

 let is_admin = M.wrap_unit @@
    ...admin code...

 let code_set code_identifier code = M.wrap @@ fun (state,context) ->
   ...code_set code...

end
```

Inside the above, the additional function `unwrap` performs the following work
it takes a value (say, a function argument) which, coming from the outside, is in the specialized type ('a M.st) and translates it to the reference type (Nucleus.st). 

Also (for instance), anything which uses `code_set` followed by ``>>`` would create a type error since `code_set` is now specialized (`t1 -> t2 -> unit st`) and we need it to be of type (`t1 -> t2 -> unit Nucleus.st`).

To do that we can use `unwrap` on `code_set` applied to all its argument except the last (we can't apply to on `code_set` *after* application of all the arguments, since at that point it may have already been evaluated (for the imperative version). So :

```
... code_set code_identifier code ... 
```

becomes 

```
... M.unwrap(code_set code_identifier) code ...
```

I haven't tried completely converting the codebase t othis format so there may be unexpected issues. I decided against it though, because it makes the code seriously cryptic (with `wrap`/`unwrap`'s) everywhere just to save a 100 lines of boilerplate code at the end of `lib/env.ml`.

# 8/4/20

## On grow constraints, possible uses :
* Rerepo: prevent any future grow which is not a rerepo of some kind
* Prevent grow after block b
* Prevent more than N grows
* Force grow to trigger payment to TL originator (eg to prepay for backwards reduction gas)
* Arbitrary "stop grow" global var, set by e.g. Dec itself

## Spend some time thinking about clocks and contract triggering mechanisms


# 3/4/2020

## Zcrossing for NFTs

At the moment the only NFTs we handle are positions & boxes. In the future there will be arbitrary NFTs. Right we do not handle zcrossing for those: you are not able to give a position you don't have to someone, then successfully complete a transaction as long as that position is in your hands eventually. 

Example (just a case of costless arbitrage applied to a position): suppose `A` wants `$20` for position `u`. And `B` would give `€20` for `u`. Assume `€:$` parity. `C` could 
1. Arrive empty-handed.
2. Flashtrade `u` against `€20` with `B`.
3. Trade `€20` for `$20`.
4. Trade `$20` for `u` with `A`.


# 1/4/2020

## General note & question about zcrossing:

** (Implemented in commit 1618b9fdba9ca9d8aa136b) **

All this nesting is at least doubling the call stack depth which is 1024 on
ethereum (and 64 with gas restrictions?) It also consumes more gas to always
bounce twice every time.

Maybe in practice people will upload a contract containing a long sequence of
commands. Then it wraps that contract in a zwrap. In that case, any 'defensive'
zwrap called along the way should probably _avoid_ bouncing through zwrap!

So the proper macro should first _test_ whether Dec is currently zwrapping! By
calling Dec.is_zwrapping. If yes, no need to 'add 1'! And thus no need for
z_nestings.


# 26/3/2020
## A way to associate code to addresses
To avoid having to split code&data declaration/definition, I could something like :

```
module MakeContract(F:functor(I:Interface) -> ContractInterface) = struct
  module Construct = F
  include F(Virtual)
end


module Contract1 = MakeContract(functor (I:Inst) ->
  struct
   let code = I.attach(code)
  end
end)

create_contract (module C) = 
let iface = new Interface in
let module _ = C.Construct(new Interface) in
iface.address
```

other solutions had no security: if you just do
```
let code = fun arg -> ...
```
then you can't tie that code to an address
and if you do
```
module Contract(I:Inst) = struct
 let code = I.attach (fun arg -> ...)
end
```
Then how do you access the key 'code' except by reapplying `Contract(Virtual)` every time?
So I think the above is nice.

Also, private code could be either declared before `module Contract1 ...`, or:
```
module MakeContract(F:functor(I:Interface) -> ContractInterface) = struct
  module Construct = F
  include F(Virtual).Public
end


module Contract1 = MakeContract(functor (I:Inst) ->
  struct
   let utility_function = ...
     module Public = struct
       let code = I.attach(code)
     end
  end
e

create_contract (module C) = 
let iface = new Interface in
let module _ = C.Construct(new Interface) in
iface
```
# 20/3/2020

## How to associate code to addresses

  another way would be to have a global 'id' counter.  every module is
associated gto an id (or maybe I can even take the module itself as a
value?)

anyway you instead of public_code, at the beginning of the module you do

```
include IsContract
let interface = Interface.new ()
let fnA = make_code(interface, fun ...)
let fnB = make_code (interface, fun ...)

let constructor = ...
```

where IsContract include a method 'construct'
which associates the current (fresh) address to 'interface'.
that way you cannot call other methods with an address -- only those registered
'construct' also calls constructor (IsContract defines a default constructor () = () ? but then only 1 return type?
you can hack it by not calling Interface.new () and getting your hands on another module's interface
*)


to automatically derive printing methods, I could use a record-based approach with

```
   module M = struct
    type t = {a: int identifier; b: string identifier}
    let store = {a = storage 0 "a"; b = storage "ok" "b"}
   end
```

then I would write `get_data M.store.a`. But the generated printing function for a `'a identifier`
would have to be of type `env -> fmt -> 'a identifier -> unit`.
Still it could be done someday!

# early march 2020
an example of what a script language could look like
```
; Re-repo example
; command prefixes:
; none means a user call
; : means surgery on the blockchain
; ~ means a script-level macro

(:time-set 0)
(:tokens google $)
(:positions u v)
(:users A B)
(:owns u (20 google))
(:owns A u v)
(:owns B (100 $))


(contract A u v 
  (forward (pay (11 $)) reduce)
  (backward (after 10) reduce)

(:trade u (10 $))

(:save-state repo)
(:time-add 5)

(:position w)
(:owns A w)

(~def fwd-clause (forward (pay (6 $)) reduce))
(contract A v w
  fwd-clause
  (backward (after 8) reduce))

(:user C)
(:owns C (75 $))
(:trade v (5 $))

(:show-state "The initial re-repo state"))
(:save-state re-repo)

; A comes back in the repo
(trigger-forward A w fwd-clause)

(:show-state "A has forwarded once")

(trigger-forward A v ((pay (11 $)) reduce))

(collect-token A u google)

(:show-state "A has forwarded twice and collected")

(:restore-state re-repo "Restoring initial re-repo state")

(:time-add 4)
(trigger-backward C v ((after 8) reduce))

(:time-add 3)

(trigger-backward B u ((after 10) reduce))

(:show-state "C has pulled; then B has pulled")

(:restore-state repo "Restore simple repo situation")

(:positions u' v')
(:owns B u' v')
(:give B u u')
(contract B u' v'
  (forward (pay (10 $)) reduce)
  (backward (after 11) reduce))
(:user C)
(:owns D (800 $))
(:trade u' (9 $))

(:show-state "Re-borrow with google asset")
(:save-state re-borrow)

; todo
; how to rehypothéquer so that {T} almost has the value of T?
; solutions: debt-based. either backing T with T-a' in debt, so C is happy
; or promising a debt of T-a' to A if T is gone when A comes back in the repo.
```

# 14/2/2020
## Should sources always be collectable?
A position contains deposits. Those deposits can be emptied through a reduction
or through a 'collection', which is a form of garbage collection. As of now,
'collectable' is defined as 'source with no next, or dead'. Is it a problem for
a source to be collectable at any time? If `A` has a position `v` to sell (with
`v` attached to a source `u` in buyer position), then buyer `B` is not
particularly at risk: either `A` cannot pull on `v`, and `B` is safe, or `A`
can pull, and `B` is unsafe whether source have to be 'killed' or not before
they become 'collectable'.


## Can relative times be salvaged by triggered timers?
We are interested in a backward condition that can express 'from time of sale
of token v + d, to...'. We suppose that the seller (`A`) has an incentive to
start the timer as early as possible, the buyer as late as possible.  Assuming
the dex address D is known advance, the timer could say: 'I can be triggered
when `ownerOf(v)` is any address != `D`'. At the time when `B` sees `v`, `v` is
owned by `D`. If `B` trusts `D`, it knows `D` will not attack by transferring
`v` to some address `D'`. So `B` can safely buy the token, and `A` is then free
to trigger the timer.

# Possible states for positions
For a position pos, here are the possible states of pos depending on
whether `tl.next pos`, `tl.dead pos`, and `tl.segments pos` are defined or not.
Whether a position `pos` is a tl source is tested by `tl.source = pos`.

  Next   Dead  Segment     Status

   ✔      ✗      ✔         alive with a next

   ✗      ✔      ✗         dead

   ✗      ✗      ✗         alive without a next

   ✔      ✔      ✔         (unreachable)

   ✗      ✗      ✔         (unreachable)

   ✗      ✔      ✔         (unreachable)

   ✔      ✔      ✗         (unreachable)

   ✔      ✗      ✗         (unreachable)

# earlier in feb 2020

## update a module with module type of

this is cool
```
module MP : sig
  include module type of Base.Map.Poly
    with type ('a,'b) t := ('a, 'b) Base.Map.Poly.t
  type ('a,'b) t
  val update_dft : ('a,'b) t -> 'a -> 'b -> f:('b -> 'b) -> ('a, 'b) t
end = struct
  include Base.Map.Poly
  let update_dft m k dv ~f = update m k ~f:(function
      | None -> f dv
      | Some v -> f v)
end
```

but a bit overkill
