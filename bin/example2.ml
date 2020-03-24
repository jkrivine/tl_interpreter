open Env


(* In this example, C1 can echo a stored phrase and can change its phrase. A
   custom authorization code lets each instanciation of C1 apply different
   rules to determine who can change its phrase *)
module C1 = struct
  (* Since the ?pp argument is missing, the phrase will be shown as '<opaque>'
     when the current environment is printed. *)
  let phrase = data ~init:"Hello" "phrase"
  let set_phrase = code ()
  let say_phrase = code ()
  (* The following code can be different for every instanciation of C1.
     This is similar to how some methods in Solidity are virtual *)
  let check_authorized = code ()

  (* The constructor takes an argument, which is itself a command.
     This command checks that the caller is authorized however it wants,
     and returns an error to reject the call. *)
  let construct auth_code =
    code_set say_phrase (fun () ->
        let* phrase_str = data_get phrase in
        echo phrase_str) >>

    code_set set_phrase (fun new_phrase_str ->
        (* `callthis` is an internal call which does not change the calling context *)
        callthis check_authorized () >>
        data_set phrase new_phrase_str) >>

    (* Here, the `auth_code` given in argument is bound to the hkey
       `check_authorized`. This binding is only valid in the address where this
       code is currently being executed. *)
    code_set check_authorized auth_code
end



let () = ignore(execute(
    let* userA = create_user "userA" in
    let* userB = create_user "userB" in
    let auth_code1 = fun () ->
      let* caller = get_caller in
      if userA = caller
      (* `return x` creates a command which takes an environment and returns
         `x` *)
      then return ()
      (* `error s` creates a command which takes an environment and raises an
         error with message `s` *)
      else error "Not authorized" in
    let* c11 = tx_create userA "c1.1" (C1.construct auth_code1) in
    let auth_code2 = fun () ->
      return () in
    let* c12 = tx_create userA "c1.2" (C1.construct auth_code2) in
    tx userA c11 C1.say_phrase () >>
    tx userA c11 C1.set_phrase "Bonjour" >>
    tx userA c11 C1.say_phrase () >>
    (* userB is not allowed to set the phrase on c11 *)
    tx userB c11 C1.set_phrase "你好" >>
    (* However, since c12 allows everyone to change the phrase, the following
       will succeed *)
    tx userB c12 C1.set_phrase "你好" >>
    tx userA c12 C1.say_phrase () >>
    echo_env
  ))
