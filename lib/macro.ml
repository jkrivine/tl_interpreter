open Env.Imp.Program
module A = Address
let zwrap dec public_identifier cmd =
  let zwrap_proxy = call dec Dec.Zwrap.get_proxy () in

  let private_identifier = code () in

  (* Macro: Ask `zwrap_proxy` to call a zprotected
     version of `private_identifier and pass along the current `caller`. `private_identifier`
     will be called at the current `this`.
  *)
  code_set public_identifier (fun args ->
      let caller = get_caller () in
      let is_zwrapping = call dec Dec.Zwrap.test () in
      if is_zwrapping
      then callthis private_identifier (caller,args)
      else Dec.Zwrap.Proxy.Magic.call_zwrap zwrap_proxy (caller,private_identifier,args)
    );

  (* Macro : run `cmd` on `args`. In addition, pass a `caller` argument to
     `cmd` which is supposed to be the caller before bouncing. It can be trusted.
  *)
  code_set private_identifier (fun (caller,args) ->
      let actual_caller = get_caller () in
      let final_caller = if actual_caller = zwrap_proxy then caller else actual_caller in
      cmd (final_caller,args)
    )

