(** The following implements bouncing through [dec] to surround a call with
   z-crossing parentheses.  We cheat a little because in OCaml we must
    deal with type safety. Since [call_zwrap] is a stored procedure, it cannot be
   polymorphic (As a ref can only be weakly polymorphic. There may be
    workarounds I'm not aware of.), so it cannot take any [('a,'b) code_id] as
   argument (to wrap the execution of the identifier in a z_(incr/decr)). We get
   around this limitation by making zwrapping a function of a module
    [ZwrapProxy.Magic].

    If a segment [s] wants to initiate a call, it may get z-crossing by
   wrapping its code between

   {[
     z_nesting_incr  >>
     <code> >>
     z_nesting_decr
   ]}

    However [Dec] cannot accept any public call to [z_nesting_incr] otherwise
   the nesting may not end with a solvency check, and a transaction could
    succeed with [Dec] still insolvent.

    This is fixed by [s] giving an identifier [hk] to [Dec] which [Dec] calls :

   {[
      z_nesting_incr >>
      call s identifier () >>
      z_nesting decr
   ]}

    Now [Dec] can safely know that whatever happens in the call, the
   transaction will not succeed without a successful solvency check.

   The above is unsatisfactory for 2 reasons:
   - Dec is calling arbitrary code with itself as caller. So we could ask Dec
   to give us all its money.
    - If a method from [s] requires a caller identity check (eg only user u can
   trigger forward), the above scheme breaks.

    To fix the first problem, [Dec] should have a proxy [z] (a [ZwrapProxy]) which implements the
    z_nesting_(incr/decr) wrapping. To fix the second, that ZwrapProxy [z] should pass a
    caller argument to the segment [s], which [s] should know it can trust.
*)

open Env.Imp.Program
module A = Address

(** Macro: [zwrap dec public_identifier cmd] binds to [public_identifier] a call to [cmd] which is zprotected by a bounce through [dec]'s zwrap_proxy. *)
val zwrap : A.t -> ('a, 'b) code_id -> (A.t * 'a -> 'b) -> unit
