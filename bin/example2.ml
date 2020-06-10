module P = Env.Imp.Program
module C = Env.Imp.Chain
(* In this example, C1 can echo a stored phrase and can change its phrase. A
   custom authorization code lets each instanciation of C1 apply different
   rules to determine who can change its phrase *)
module C1 = struct

  (* Since the ?pp argument is missing, the phrase will be shown as '<opaque>'
     when the current environment is printed. *)
  let phrase = P.data "phrase"

  let set_phrase = P.code ()
  let say_phrase = P.code ()

  (* The following code can be different for every instanciation of C1.
     This is similar to how some methods in Solidity are virtual *)
  let check_authorized = P.code ()

  (* The constructor takes an argument, which is itself a command.
     This command checks that the caller is authorized however it wants,
     and returns an error to reject the call. *)
  let construct auth_code =

    P.data_set phrase "Hello";

    P.code_set say_phrase begin
      fun () ->
        let phrase_str = P.data_get phrase in
        P.echo phrase_str
    end;

    P.code_set set_phrase begin
      fun new_phrase_str ->
        (* `callthis` is an internal call which does not change the calling context *)
        P.callthis check_authorized ();
        P.data_set phrase new_phrase_str
    end;

    (* Here, the `auth_code` given in argument is bound to the identifier
       `check_authorized`. This binding is only valid in the address where this
       code is currently being executed. *)
    P.code_set check_authorized auth_code
end



let () =

  let userA = P.create_user "userA" in
  let userB = P.create_user "userB" in

  let auth_code1 = fun () ->
    let caller = P.get_caller () in
    if userA = caller
    (* `return x` creates a command which takes an environment and returns
       `x` *)
    then C.return ()
    (* `error s` creates a command which takes an environment and raises an
       error with message `s` *)
    else C.error "Not authorized" in
  let c11 =
    C.tx_create userA "c1.1" C1.construct auth_code1 in

  let auth_code2 = fun () ->
    C.return () in
  let c12 =
    C.tx_create userA "c1.2" C1.construct auth_code2 in

  C.tx userA c11 C1.say_phrase ();
  C.tx userA c11 C1.set_phrase "Bonjour";
  C.tx userA c11 C1.say_phrase ();

  (* userB is not allowed to set the phrase on c11 *)
  C.tx userB c11 C1.set_phrase "你好";

  (* However, since c12 allows everyone to change the phrase, the following
     will succeed *)
  C.tx userB c12 C1.set_phrase "你好";
  C.tx userA c12 C1.say_phrase ();
  C.echo_env ()
