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
    type t = {a: int hkey; b: string hkey}
    let store = {a = storage 0 "a"; b = storage "ok" "b"}
   end
```

then I would write `get_data M.store.a`. But the generated printing function for a `'a hkey`
would have to be of type `env -> fmt -> 'a hkey -> unit`.
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
