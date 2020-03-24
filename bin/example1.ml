open Env

(* We define an onchain contract. All public data and code are members of the
   module structure. The following contract holds a counter that can be
   incremented and decremented by anyone. The contract can also say hi. *)
(* Contracts can be deployed at multiple addresses; each will have its own storage space *)
module Contract = struct
  (* `data ?init ?pp name` declares a new data storage location.
     ?init is an optional argument which gives it an initial value.
     It is also possible to initialize the data when the contract is instantiated, using
     `data_set`.

     ?pp is a custom printing function which should have the type
     `Format.formatter -> t -> unit`
     where `t` is the type of the data to be held in storage.
     See the file PRINTING for some help.
   *)
  let counter = data ~init:0 ~pp:Format.pp_print_int "counter"

  (* `code ()` declares a new code storage location.
     The type wil usually be inferred; if not, the typechecker will
     complain about non-generalizable types. In that case, add a type
     annotation to help the typechecker. *)
  let incr = code ()
  let decr = code ()
  let hello = code ()

  (* Note that the `data` and `code` functions return _relative storage
     locations_, that is, storage locations which depend on the current
     address. In OCaml code, they are keys of a heterogenous map. These keys
     are reused in multiple heterogenous maps, for every instanciation of the
     contract. We will call those locations (whether for code or storage)
     `hkey`s. *)

  (* `construct` is just a generic name for the constructor. New instances of
     the contract should be created by calling construct *)
  let construct =
    (* the following code will be executed in an environment with
       * a current, global chain state
       * an execution context, which includes the current address
       *)

    (* `code_set` takes an hkey and a function, and binds that function
       to that hkey in the current address' storage space *)

    (* For instance here, we bind a function which updates the int associated
       with the `counter` hkey to the `incr` hkey. *)
    code_set incr begin fun () ->
      data_update counter (fun c -> c+1)
    end >>

    (* Note the `>>` which acts as a semicolon between statements. *)

    code_set decr begin fun () ->
      data_update counter (fun c -> c-1)
    end >>

    code_set hello begin fun () ->
      (* echo is the within-blockchain version of 'print' *)
      echo "Hello from Contract"
    end
end



let () = ignore ( execute (
    (* The following creates a new user. Technically it takes a new address and
       sets its code and storage to "nothing". *)

    (* The `let*` is syntactic sugar. Blockchain commands expect an environment
       to be executed; they may return values. The `let*` binds the future
       return value of the command to a variable name. *)
    let* userA = create_user "userA" in
    (* userA creates two instances of `Contract`. `tx_create` takes an
       originating address, an instance name (a string) and a command. The
       command is executed under a fresh address with special permissions (it
       can bind code to storage addresses); that fresh address is returned for
       convenience. *)
    let* c = tx_create userA "c" Contract.construct in
    let* c' = tx_create userA "c'" Contract.construct in
    (* `tx` initiates a normal transaction. It takes an originating address, a
       contract address, an hkey and appropriate arguments *)
    tx userA c Contract.incr () >>
    (* `echo_env` pretty-prints the current state (global storage) and calling context.
       The commands `echo_state` and `echo_context` are also avialable. *)
    echo_env >>
    (* Note that the same hkey is given to two different instances of the
       contract. Similarly, in solidity, a same function digest can be used to
       call two different addresses *)
    tx userA c Contract.incr () >>
    tx userA c' Contract.decr () >>
    echo_env >>
    (* When `echo` is called from within a contract, the terminal will show
       where the printing emanated from with a prefix of the form
       ⦃ address_name ⦄ ⇒ ...
    *)
    tx userA c Contract.hello () >>
    tx userA c' Contract.hello ()
  ))
