open Env
module A = Address
(* The following implements bouncing through `dec` to surround a call with
   z-crossing parentheses.  We do it in an especially ugly way because, in
   this OCaml version, we must deal with type safety. Since `z_protect` is a
   stored procedure, it cannot be polymorphic (As a ref can only be weakly
   polymorphic. There may be workarounds I'm not aware of.), so it cannot
   take any `('a,'b) code_hkey` as argument (to wrap the execution of the
   hkey in a z_(incr/decr)). We get around this limitation in a way which
   would also work in Solidity: we define two variables `_in`, `_out` (in
   Solidity: getter/setters), `_in` for input data, which is set before
   calling the function. That function sets `_out` (for output data) and
   returns unit.  Thus z_protect can have type `((unit,unit) code_hkey,unit)
   code_hkey`.

   If a segment `s` wants to initiate a call, it may get z-crossing by
   wrapping its code between

   ``` z_nesting_incr <code> z_nesting_decr ```

   However Dec cannot accept any public call to `z_nesting_incr` otherwise
   the nesting may not end with a solvency check, and a transaction could
   succeed with Dec still insolvent.

   This is fixed by `s` giving an hkey `hk` to `Dec` which `Dec` calls :

   ``` z_nesting_incr call s hkey () z_nesting decr ```

   Now `Dec` can safely know that whatever happens in the call, the
   transaction will not succeed without a successful solvency check.

   The above is unsatisfactory for 2 reasons: 1) Dec is calling arbitrary
   code with its own identity 2) If a method from `s` requires a caller
   identity check (eg only user u can trigger forward), the above scheme
   breaks.

   To fix 1), Dec should have a proxy `p` which implements the
   z_nesting_(incr/decr) wrapping. to fix 2), that proxy `p` should pass a
   caller argument to the segment `s`, which `s` should know it can trust.
*)

(* Macro: given two hkeys `_in` and `_out`, ask `proxy` to call a zprotected
   version of `private_hkey and pass along the current `caller`. `private_hkey`
   will be called at the current `this`.

   `_in` receives `args` before the call so that they can be read later, and
   `_out` is read after, assuming the call has set it to some return value.
   The little `_in`/`_out` dance is due to lack of polymorphism in stored
   hkeys.  Otherwise we would just pass arguments along and get the return
   value directly. *)
let bounce_call proxy _in private_hkey _out = fun args ->
  let* caller = Env.get_caller in
  Env.data_set _in args >>
  Env.call proxy Dec.Proxy.bounce (caller,private_hkey)
  >> Env.data_get _out

(* Macro : given two hkeys `_in` and `_out`, run `cmd` on the value of `_in`
   and set `_out` to its return value. In addition, pass a `caller` argument to
   `cmd` which is supposed to be the caller before bouncing. It can be trusted.
*)
let receive_bounced_call proxy _in cmd _out = fun caller ->
  let* actual_caller = Env.get_caller in
  Env.require (Env.return (actual_caller = proxy)) >>
  let* args = Env.data_get _in in
  let* ret = cmd (caller,args) in
  Env.data_set _out ret

(* Macro: bind to `public_hkey` a call to `cmd` which is zprotected by a bounce
   through `proxy`. *)
let z_protect proxy public_hkey cmd =
  let _in = Env.data_hidden ()
  and _out = Env.data_hidden ()
  and private_hkey = Env.code () in
  Env.code_set public_hkey (bounce_call proxy _in private_hkey _out) >>
  Env.code_set private_hkey (receive_bounced_call proxy _in cmd _out)
