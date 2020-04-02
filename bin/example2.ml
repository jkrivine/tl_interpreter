let (let*) = Env.(let*)
let (>>) = Env.(>>)

(* In this example, C1 can echo a stored phrase and can change its phrase. A
   custom authorization code lets each instanciation of C1 apply different
   rules to determine who can change its phrase *)
module C1 = struct

  (* Since the ?pp argument is missing, the phrase will be shown as '<opaque>'
     when the current environment is printed. *)
  let phrase = Env.data ~init:"Hello" "phrase"

  let set_phrase = Env.code ()
  let say_phrase = Env.code ()

  (* The following code can be different for every instanciation of C1.
     This is similar to how some methods in Solidity are virtual *)
  let check_authorized = Env.code ()

  (* The constructor takes an argument, which is itself a command.
     This command checks that the caller is authorized however it wants,
     and returns an error to reject the call. *)
  let construct auth_code =

    Env.code_set say_phrase begin fun () ->
        let* phrase_str = Env.data_get phrase in
        Env.echo phrase_str
    end >>

    Env.code_set set_phrase begin fun new_phrase_str ->
        (* `callthis` is an internal call which does not change the calling context *)
        Env.callthis check_authorized () >>
        Env.data_set phrase new_phrase_str
    end >>

    (* Here, the `auth_code` given in argument is bound to the hkey
       `check_authorized`. This binding is only valid in the address where this
       code is currently being executed. *)
    Env.code_set check_authorized auth_code
end



let () = ignore(Env.execute(

    let* userA = Env.create_user "userA" in
    let* userB = Env.create_user "userB" in

    let auth_code1 = fun () ->
      let* caller = Env.get_caller in
      if userA = caller
      (* `return x` creates a command which takes an environment and returns
         `x` *)
      then Env.return ()
      (* `error s` creates a command which takes an environment and raises an
         error with message `s` *)
      else Env.error "Not authorized" in
    let* c11 =
      Env.tx_create userA "c1.1" (C1.construct auth_code1) in

    let auth_code2 = fun () ->
      Env.return () in
    let* c12 =
      Env.tx_create userA "c1.2" (C1.construct auth_code2) in

    Env.tx userA c11 C1.say_phrase () >>
    Env.tx userA c11 C1.set_phrase "Bonjour" >>
    Env.tx userA c11 C1.say_phrase () >>

    (* userB is not allowed to set the phrase on c11 *)
    Env.tx userB c11 C1.set_phrase "你好" >>

    (* However, since c12 allows everyone to change the phrase, the following
       will succeed *)
    Env.tx userB c12 C1.set_phrase "你好" >>
    Env.tx userA c12 C1.say_phrase () >>
    Env.echo_env
  ))
