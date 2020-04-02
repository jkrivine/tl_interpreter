open Env
module A = Address
(* The following implements bouncing through `dec` to surround a call with
   z-crossing parentheses.  We cheat a little because here in OCaml we must
   deal with type safety. Since `call_zwrap is a stored procedure, it cannot be
   polymorphic (As a ref can only be weakly polymorphic. There may be
   workarounds I'm not aware of.), so it cannot take any `('a,'b) code_hkey` as
   argument (to wrap the execution of the hkey in a z_(incr/decr)). We get
   around this limitation by making zwrapping a function of a module
   Zwrapper.Magic.

   If a segment `s` wants to initiate a call, it may get z-crossing by
   wrapping its code between

   ```
   z_nesting_incr  >>
   <code> >>
   z_nesting_decr
   ```

   However Dec cannot accept any public call to `z_nesting_incr` otherwise
   the nesting may not end with a solvency check, and a transaction could
   succeed with Dec still insolvent.

   This is fixed by `s` giving an hkey `hk` to `Dec` which `Dec` calls :

   ```
   z_nesting_incr >>
   call s hkey () >>
   z_nesting decr
   ```

   Now `Dec` can safely know that whatever happens in the call, the
   transaction will not succeed without a successful solvency check.

   The above is unsatisfactory for 2 reasons:
   1) Dec is calling arbitrary code with itself as caller. So we could ask Dec
   to give us all its money.
   2) If a method from `s` requires a caller identity check (eg only user u can
   trigger forward), the above scheme breaks.

   To fix 1), Dec should have a proxy `z` (a 'zwrapper') which implements the
   z_nesting_(incr/decr) wrapping. to fix 2), that zwrapper `z` should pass a
   caller argument to the segment `s`, which `s` should know it can trust.
*)

(* Macro: Ask `zwrapper to call a zprotected
   version of `private_hkey and pass along the current `caller`. `private_hkey`
   will be called at the current `this`.
*)
let zwrap_launch zwrapper private_hkey = fun args ->
  let* caller = Env.get_caller in
  Dec.Zwrapper.Magic.call_zwrap zwrapper (caller,private_hkey,args)

(* Macro : run `cmd` on `args`. In addition, pass a `caller` argument to
   `cmd` which is supposed to be the caller before bouncing. It can be trusted.
*)
let zwrap_receive zwrapper cmd = fun (caller,args) ->
  let* actual_caller = Env.get_caller in
  Env.require (Env.return (actual_caller = zwrapper)) >>
  cmd (caller,args)

(* Macro: bind to `public_hkey` a call to `cmd` which is zprotected by a bounce
   through zwrapper. *)
let zwrap zwrapper public_hkey cmd =
  let private_hkey = Env.code () in
  Env.code_set public_hkey (zwrap_launch zwrapper private_hkey) >>
  Env.code_set private_hkey (zwrap_receive zwrapper cmd)
