module P = Env.Fun.Program

open P

module Auth = struct
  type 'a response = Authorized of 'a | Forbidden
  let check = code ()
  let change_owner : (unit,unit) code_id = code ()
  (* functions defined here are [public], in solidity terms *)
  (* we don't do [external] (only from outside) *)
  (* [internal] (only from self and inheriting contracts)  can be returned from the construct *)
  let owner = data "owner"


  let construct address =
    (* functions defined here are [private] in solidity terms: only this contract can call them, internally *)
    let code = (fun new_owner ->
        callthis check () >>
        data_set owner new_owner) in
    code_private code >>= fun change_owner ->
    data_set owner address >>
    code_set check (fun () ->
        get_caller >>= fun caller ->
        if caller = address
        then return (Authorized ())
        else return Forbidden) >>
    return change_owner

end


module PermissionedCounter = struct
  let add = code ()
  let read = code ()
  let addread = code ()

  let construct (owner,initial_value) =
    data_private initial_value >>= fun counter ->
    import (Auth.construct owner) >>= fun _change_owner ->
    code_set add (fun y -> callthis Auth.check () >>= function
      | Auth.Authorized _ as yes ->
        data_get counter >>= (fun x -> data_set counter (x+y)>> return yes)
      | Auth.Forbidden as no ->
        return no) >>
    code_set read (fun () -> data_get counter) >>
    code_set addread (fun y -> callthis add y >> callthis read ())
end

let say_hi = code ()

let polyglot_construct () =
  code_set say_hi (fun () -> return (List.nth ["Hello";"Hola";"Bonjour"] (Random.int 3)))

module MyContract = struct
  let say_hello_and_count = code ()
  let construct speaker_address =
    get_this >>= fun this ->
    create_contract "permissioned_counter" PermissionedCounter.construct (this,0) >>=
    fun counter_address ->
    code_set say_hello_and_count (fun () ->
        call counter_address PermissionedCounter.add 1 >>
        let* text = call speaker_address say_hi () in
        echo text >> return text
      )
end

module Virtual = struct
  let value : int data_id = data "value"
  let act : (int,unit) code_id = code ()
  let construct act_code =
    code_set act (act_code value) >>
    return (data_get value)
end

(* begin calls *)

open Env.Fun.Chain
let () = ignore(execute(
let* userA = create_user "uA" in
let* polyglot_address = tx_create userA "polyglot" polyglot_construct () in
let* my_contract_address = tx_create userA "myc" MyContract.construct polyglot_address in
let* _response = tx userA my_contract_address MyContract.say_hello_and_count () in
return ()))


