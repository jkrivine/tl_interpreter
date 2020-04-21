open Imperative.P
module A = Address
(* The following implements bouncing through `dec` to surround a call with
   z-crossing parentheses.  We cheat a little because here in OCaml we must
   deal with type safety. Since `call_zwrap is a stored procedure, it cannot be
   polymorphic (As a ref can only be weakly polymorphic. There may be
   workarounds I'm not aware of.), so it cannot take any `('a,'b) code_hkey` as
   argument (to wrap the execution of the hkey in a z_(incr/decr)). We get
   around this limitation by making zwrapping a function of a module
   ZwrapProxy.Magic.

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

   To fix 1), Dec should have a proxy `z` (a 'ZwrapProxy) which implements the
   z_nesting_(incr/decr) wrapping. to fix 2), that ZwrapProxy `z` should pass a
   caller argument to the segment `s`, which `s` should know it can trust.
*)

(* Macro: bind to `public_hkey` a call to `cmd` which is zprotected by a bounce
   through zwrap_proxy. *)
let zwrap dec public_hkey cmd =
  let zwrap_proxy = call dec Dec.Zwrap.get_proxy () in

  let private_hkey = code () in

  (* Macro: Ask `zwrap_proxy` to call a zprotected
     version of `private_hkey and pass along the current `caller`. `private_hkey`
     will be called at the current `this`.
  *)
  code_set public_hkey (fun args ->
      let caller = get_caller () in
      let is_zwrapping = call dec Dec.Zwrap.test () in
      if is_zwrapping
      then callthis private_hkey (caller,args)
      else Dec.Zwrap.Proxy.Magic.call_zwrap zwrap_proxy (caller,private_hkey,args)
    );

  (* Macro : run `cmd` on `args`. In addition, pass a `caller` argument to
     `cmd` which is supposed to be the caller before bouncing. It can be trusted.
  *)
  code_set private_hkey (fun (caller,args) ->
      let actual_caller = get_caller () in
      let final_caller = if actual_caller = zwrap_proxy then caller else actual_caller in
      cmd (final_caller,args)
    )

