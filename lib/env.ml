(* The general theme here is: contracts are executed in a context specifying
   `this`, which is an address.  There is a global storage which associates a
   heterogenous map to each address.  This map can contain data, code, etc.

   Given a key `k: int key`, which is a key to get a value of type int, the
   result of accessing k depends on the current value of `this`. Since the
   context is hidden in the pseudo-state monad, contracts can just say `read
   k`. 2 instances of the same contract at 2 different addresses will return a
   different value.

   Those keys also map to code. So if there are two instances of a contract
   `C`, one at address `ad1` and the other at address `ad2`, and `C` exposes
   the key `k: (int -> int) key`, running `call ad1 C.k` and `call ad2 C.k` may
   not run the same code.  *)

  (*
   * Global chain stuff
   *)

(* Global chain storage *)
(* The global storage is of the form address -> 'a key -> 'a *)
(* Where 'a key is the type of map keys for a data of type 'a *)


module Nucleus = struct
  open Tools

  module Types = struct
    type storage = (Address.t,HM.t) MP.t
    type state = {
      hmaps: storage;
      time: int;
    }
    type context = {
      caller: Address.t;
      this: Address.t;
      constructor: bool
    }
    type env = state*context
    type 'a st = env -> ( ('a,(string*context*Printexc.raw_backtrace)) result * state )
    type ('a,'b) code_identifier = ('a -> 'b st) HM.key
    type 'a data_identifier = 'a HM.key
  end

  include Types

  module Monad = struct
    type 'a st = 'a Types.st
    type 'a unit_st = 'a Types.st
    type ('a,'b) code_identifier = ('a,'b) Types.code_identifier
    type 'a data_identifier = 'a Types.data_identifier

    let bind t1 t2 =
      fun (s,c) -> match t1 (s,c) with
        | (Ok v,s') -> t2 v (s',c)
        | (Error (v,c,cs),s') -> (Error (v,c,cs),s')
    let (>>=) = bind
    let ( let* ) t1 t2 = bind t1 t2
    let (>>) t1 t2 = bind t1 (fun _ -> t2)
    let return v = fun (s,_) -> (Ok v,s)
    let error v = fun (s,c) -> (Error (v,c,Printexc.get_callstack 10),s)
    let (|?*) a default = a >>= function Some e -> return e | None -> return default
  end

  include Monad

  let empty_state = {
    hmaps = MP.empty;
    time = 0
  }


  let pp_state fmt state =
    F.p fmt "State at time %d:" state.time;
    F.cr ();
    MP.pp Address.pp HM.pp fmt state.hmaps;
    F.p fmt "\n"


  let empty_context = {
    caller = Address.admin;
    this = Address.admin;
    constructor = false
  }

  let pp_context fmt {caller;this;constructor}=
    F.p fmt "Context:";
    F.with_indent (fun () ->
        F.cr ();
        F.p fmt "caller: %a" Address.pp caller; F.cr ();
        F.p fmt "this: %a" Address.pp this;);
    (if constructor
     then
       F.p fmt " in constructor"
     else ())

  (* An execution environment specifies the current state and calling context *)

  let empty_env =
    (empty_state,empty_context)

  let state_get = fst
  let state_set e s = (s,snd e)
  let context_get = snd

  (*
   * Pseudo-state monad stuff
   *)

  (* In-chain commands take an environment as input (ie. a (state,context) pair)
     and return a (value result,state) pair as output *)


  let _require_admin context = context.this = Address.admin
  let _require_admin_caller context = context.caller = Address.admin

  (* Admin-only stuff *)
  (* Execution at the 'root' is from the admin address.
     Anything nested below that is by a user or a contract
     The admin address can execute some privileged functions *)
  let ite_admin t1 t2 (state,context) =
    (if _require_admin context then t1 () else t2 ()) (state,context)

  let ite_admin_caller t1 t2 (state,context) =
    (if _require_admin_caller context then t1 () else t2 ()) (state,context)

  let is_equal t1 t2 =
    let* r1 = t1 in
    let* r2 = t2 in
    return (r1 = r2)


  (*
   * Low-level read/write in global storage
   *)

  (* Get the hmap associated with the current address *)
  (* Creates hmap if needed *)
  (* Direct access version *)
  let _get_hmap address hmaps = match MP.find hmaps address with
    | Some hmap -> hmap
    | None -> HM.empty
  let get_hmap (state,context) =
    (Ok (_get_hmap context.this state.hmaps),state)

  (* Set the hmap associated with the current address *)
  let set_hmap hmap ({hmaps; _} as state,context) =
    let hmaps' = MP.set hmaps context.this hmap in
    (Ok (),{state with hmaps=hmaps'})


  (* Get value associated with [identifier] in this context's hmap *)
  (* Direct access version *)
  let _get_in_hmap_option identifier address hmaps = HM.find identifier (_get_hmap address hmaps)

  let get_in_hmap_option identifier (state,context) =
    (Ok (_get_in_hmap_option identifier context.this state.hmaps), state)

  (* Get value associated with [identifier] in this context's hmap *)
  let get_in_hmap identifier =
    get_in_hmap_option identifier >>= function
    | Some v -> return v
    | None ->
      error ("No such identifier: "^((HM.Key.info identifier).name))

  (* Set value associated with [identifier] in this context's hmap *)
  let set_in_hmap identifier v =
    get_hmap >>= fun hmap ->
    let hmap' = HM.add identifier v hmap in
    set_hmap hmap'



  (* Data *)
  (* Code and data keys are treated differently because,
     while data may be updated at any time, we only allow setting the
     code associated with a key (at an address) during the construction of
     the contract *)
  (* security-wise, we can add any unset key to hmap of a contract by giving it a
     key with a default value. It shouldn't be able to do anything with it
     though, we just use its data for free *)
  (* State saving/restoring *)
  type history = (string,state) MP.t ref
  let history : history = ref MP.empty
end

module Program = struct
  include Nucleus.Monad
  open Nucleus
  open Tools

  let is_admin =
    ite_admin (fun () -> return true) (fun () -> return false)

  let is_admin_caller =
    ite_admin_caller (fun () -> return true) (fun () -> return false)

  let require_admin : unit st =
    ite_admin (fun () -> return ()) (fun () -> error "not admin")

  let require_admin_s s : unit st =
    ite_admin (fun () -> return ()) (fun () -> error ("not admin ("^s^")"))

  (* Initialize a new key for code *)
  let code () = HM.Key.create {name="<code>";pp=None;hidden=true}

  let code_set code_identifier code (state,context) =
    if context.constructor then
      (get_in_hmap_option code_identifier >>= function
        | Some _ -> error "Code already set at this identifier"
        | None -> set_in_hmap code_identifier code ) (state,context)
    else
      (Error ("Cannot set code outside of constructor",context,Printexc.get_callstack 10), state)

  let code_private f (state,context) =
    if context.constructor then
      (let k = code () in code_set k f >> return k) (state,context)
    else
      (Error ("Cannot declare private code outside of constructor",context,Printexc.get_callstack 10), state)

  let data ?pp name = HM.Key.create {name;pp;hidden=false}
  let data_hidden () = HM.Key.create {name="<hidden>";pp=None;hidden=true}

  let data_set (data_identifier: 'a data_identifier) (v:'a) = set_in_hmap data_identifier v

  let data_get data_identifier = get_in_hmap data_identifier

  (* Simple read-and-write convenience *)
  let data_update data_identifier f =
    let* v = data_get data_identifier in
    set_in_hmap data_identifier (f v)

  (* Convenience: define data that will only be visible to
     - constructor methods
     - any inheriting contract if the key is returned by the constructor
  *)
  let data_private v (state,context) =
    if context.constructor then
      (let d = data "<private>" in data_set d v >> return d) (state,context)
    else
      (error "Cannot declare private data outside of constructor") (state,context)


(*
 * Map-specific convenience functions
 *)

  (* Consider [identifier] as some [map]'s name. Set the value of [k] in [map] to [v] *)
  let map_set data_identifier k v =
    data_update data_identifier (fun m -> MP.set m k v)

  let map_remove data_identifier k =
    data_update data_identifier (fun m -> MP.remove m k)

  (* Consider [identifier] as some [map]'s name. Get the value of [k] in [map] *)
  let map_find data_identifier k =
    data_get data_identifier >>= fun map -> return (MP.find map k)

  let map_find_exns s data_identifier k =
    map_find data_identifier k >>= function Some v -> return v | None -> error s

  let map_find_exn data_identifier k =
    map_find_exns "Not found" data_identifier k

  exception BadUpdate
  let map_update data_identifier k ?default f =
    try
      data_update data_identifier (fun m ->
          let v = match (MP.find m k,default) with
            | (Some v, _) |(None, Some v) -> v
            | (None,None) -> raise BadUpdate in
          MP.set m k (f v))
    with BadUpdate ->
      error "Cannot update non-existent mapping without a default"


  (* Utilities *)
  (* Get context info from inside the execution *)
  let get_caller (state,context) = (Ok context.caller,state)
  let get_this (state,context) = (Ok context.this,state)

  (* Time stuff, some of it admin-only *)
  let time_get (state,_) =
    (Ok state.time,state)


  (* High-level chain method *)
  (* import creates additional keys at the same address *)
  (* internal call is just calling the method. otherwise do call or delegatcall *)

  (* Run an instruction sequence on the empty environment *)


  (* Run `f` at a fresh address in a constructor context *)
  let create_contract name f args (state,context) =
    let address = Address.next name in
    let hmaps' = MP.set state.hmaps address HM.empty in
    let state' = {state with hmaps=hmaps'} in
    let context' = ({constructor=true;this=address;caller=context.this}) in
    (f args >>= fun _ -> return address) (state',context')

  let create_empty_contract name (state,context) =
    (create_contract name (fun () -> return ()) ()) (state,context)

  let create_user = create_empty_contract

  (* A cooler alternative but it's annoying to write at call site :
     create_contract (val Module) (args)
     plus the arguments have to be packed into a tuple *)
  (* Run `C.construct` at a fresh address in a constructor context *)
  module type Contract = sig
    type i
    type o
    val construct : i -> o st
  end

  let create_contract'
      (type a) (type b) name
      (module C : Contract with type i = a and type o = b)
      (args:a) (state,context) =
    let address = Address.next name in
    let hmaps' = MP.set state.hmaps address HM.empty in
    let state' = {state with hmaps=hmaps'} in
    let context' = ({constructor=true;this=address;caller=context.this}) in
    ((C.construct args) >>= fun ret -> return (address,ret)) (state',context')
  [@@ocaml.warning "-32"]

  (* Get the code given by `code_identifier` at `address`, run it in *context'` instead
     of the current context *)
  let _call context' address code_identifier args (state,context) =
    match _get_in_hmap_option code_identifier address state.hmaps with
    | Some code -> code args (state,context')
    | None -> (Error ("Code not found",context,Printexc.get_callstack 10),state)

  (* Get the code given by `code_identifier` at `address`, run it at `address` *)
  let call address code_identifier args (state,context) =
    let context' = ({constructor=false; this=address;caller=context.this}) in
    _call context' address code_identifier args (state,context)

  (* Get the code given by `code_identifier` at `address` but run it in the current context *)
  let delegatecall address code_identifier args (state,context) =
    _call context address code_identifier args (state,context)

  (* Convenience, do a call at current address *)
  let callthis chk args (state,context) =
    delegatecall context.this chk args (state,context)

  let require b =
    b >>= function true -> return () | false -> error "require failed"

  (* Run the given code (only allowed in a constructor, it is used to simulate
     inheritance but `inherit` is a reserved keyword, so we use `import` *)
  let import f (state,context) =
    if context.constructor = true
    then f (state,context)
    else (Error ("cannot inherit outside of constructor",context,Printexc.get_callstack 10),state)

  let proxy address ?caller f =
    require_admin_s "proxy" >> fun (state,context) ->
    f (state,{context with this=address; caller=(caller |? context.caller)})

  (* Check if a contract has an entry for key `k` *)
  let responds address (k:('a,'b) code_identifier) : bool st = fun (state,context) ->
    (match _get_in_hmap_option k address state.hmaps with
     | Some c -> return true
     | None -> return false) (state,context)

  module Echo = struct
    (*
     * 'echo' is printing from within the execution
     *)

    (* Echo a string *)
    (* (state,context) arg important for evaluation time of F.p functions *)
    let echo str (state,context) =
      (ite_admin
         (fun () -> return (F.p Format.std_formatter "%s" str;F.cr ()))
         (fun () -> return (F.p Format.std_formatter "%a | %a ⇒ " Address.pp context.caller Address.pp context.this;
                            F.p Format.std_formatter "%s" str;F.cr ())))
        (state,context)

    let echo_data k =
      let* v = data_get k in
      echo (pp_to_str HM.pp_binding (HM.B (k,v)))

    let echo_pp s = Format.kfprintf (fun _ -> return ()) Format.std_formatter s

    let echo_address a =
      echo (Address.to_string a)

    let echo_trace (state,context) =
        (return (F.p Format.std_formatter "%s"
          (Printexc.raw_backtrace_to_string @@ Printexc.get_callstack 10))) (state,context)

    (* Echo the current state *)
    let echo_state (state,context) =
      (return (pp_state Format.std_formatter state)) (state,context)

    (* Echo the current context *)
    let echo_context (state,context) =
      (return (pp_context Format.std_formatter context)) (state,context)

    (* Pretty formatting for current (state,context) pair *)
    let echo_env =
      let line = "───────────────────────────────────────────" in
      (fun e -> F.with_indent (fun () ->
           (echo ("╭"^line^"╮") >>
            echo_state) e)) >>
      echo "" >>
      (fun e -> F.with_indent (fun () ->
           (echo ("├"^line^"┤")>>
            echo_context) e)) >>
      echo "" >> echo ("╰"^line^"╯")
  end
  include Echo
end

module Chain = struct
  (* Initialize a new key for data *)


  type storage = Nucleus.storage
  type state = Nucleus.state
  type context = Nucleus.context
  type env = Nucleus.env

  open Nucleus
  open Tools

  include Nucleus.Monad

  open Program

  let execute f =
    match f empty_env with
    | (Ok v,_) -> Some v
    | (Error _,_) -> None

  (* User sends a transactions *)
  let txr user address code_identifier args =
    require_admin_s "tx_user" >> fun (state ,context) ->
    (call address code_identifier args) (state,{context with this=user})

  let tx user address code_identifier args (state,context) =
    match txr user address code_identifier args (state,context) with
    | (Ok _, new_state) -> (Ok (), new_state)
    | (Error (v,c,callstack),_) ->
        let sf = Format.std_formatter in
        F.p sf "Error: %s" v;
        F.cr ();
        F.p sf "%s" "In ";
        pp_context sf c;
        F.cr ();
        F.p sf "%s" (Printexc.raw_backtrace_to_string @@ callstack);
        F.cr ();
        F.p sf "Reverting state...";
        F.cr ();
        (Ok (), state)

  let tx_create user name t args =
    require_admin_s "tx_create" >> fun (state,context) ->
    (create_contract name t args) (state,{context with this=user})

  (* not natively available in ethereum, included for convenience *)
  let tx_proxy user code = proxy user code

  let time_set i =
    require_admin_s "time_set" >> (fun (state,_context) ->
        (Ok (),{state with time=i}))

  let time_incr i =
    require_admin_s "time_incr" >> fun (state,_context) ->
    (Ok (),{state with time=state.time+i})

  let state_save name =
    require_admin_s "state_save" >> fun (state,context) ->
    (return (history := MP.set !history name state)) (state,context)

  let state_restore name =
    require_admin_s "state_restore" >> fun _env ->
    let state' = MP.find_exn !history name in
    (Ok (),state')

  include Echo
end


module Imp = struct
  open Tools

  let env = ref Nucleus.empty_env

  exception EnvException of string*Nucleus.context*Printexc.raw_backtrace

  let () = Printexc.register_printer (function
      | EnvException (s,c,cs) ->
        Some (s^"\n"^pp_to_str Nucleus.pp_context c^"\n"^(Printexc.raw_backtrace_to_string cs))
      | _ -> None)

  let imp f =
    match f !env with
    | (Ok v,s') -> env := Nucleus.state_set !env s'; v
    | (Error (m,c,cs),s') -> env := Nucleus.state_set !env s';
      raise (EnvException (m,c,cs))

  let unimp f args = fun env' ->
    let env_bk = !env in
    env := env';
    try
      let v = f args in
      let s' = Nucleus.state_get !env in
      env := (Nucleus.state_set env_bk s');
      (Ok v, s')
    with
    | EnvException (m,e,cs) ->
      env := env_bk;
      (Error (m,e,cs), Nucleus.state_get !env)

  module Monad = struct
    type 'a data_identifier = 'a Nucleus.data_identifier
    type ('a,'b) code_identifier = ('a,'b) Nucleus.code_identifier

    let bind t1 t2 = t2 t1
    let (>>=) = bind
    let ( let* ) t1 t2 = bind t1 t2
    let (>>) t1 t2 =  (ignore t1) ; t2
    let return v = v
    let error s = raise (EnvException (s,Nucleus.context_get !env,Printexc.get_callstack 10))
    let (|?*) = Tools.(|?)
  end

  module FP = Program
  module FC = Chain

  module Program = struct
    include Monad

    let code = FP.code

    let code_set code_identifier code = imp @@ FP.code_set code_identifier (unimp code)

    let code_private code = imp @@ FP.code_private (unimp code)

    let data = FP.data

    let data_set data_identifier v = imp @@ FP.data_set data_identifier v

    let data_get data_identifier = imp @@ FP.data_get data_identifier

    let data_update data_identifier f = imp @@ FP.data_update data_identifier f

    let data_private v  = imp @@ FP.data_private v

    let map_set data_identifier k v = imp @@ FP.map_set data_identifier k v

    let map_remove data_identifier k = imp @@ FP.map_remove data_identifier k

    let map_find data_identifier k = imp @@ FP.map_find data_identifier k

    let map_find_exns s data_identifier k = imp @@ FP.map_find_exns s data_identifier k

    let map_find_exn data_identifier k = imp @@ FP.map_find_exn data_identifier k

    exception BadUpdate

    let map_update data_identifier k ?default f =
      match default with
      | Some default -> imp @@ FP.map_update data_identifier k ~default f
      | None -> imp @@ FP.map_update data_identifier k f

    let get_caller () = imp @@ FP.get_caller

    let get_this () = imp @@ FP.get_this

    let create_contract name f args = imp @@ FP.create_contract name (unimp f) args

    let create_empty_contract name = imp @@ FP.create_empty_contract name
    let create_user = create_empty_contract

    let _call context' address code_identifier args = imp @@ FP._call context' address code_identifier args

    let call address code_identifier args = imp @@ FP.call address code_identifier args

    let delegatecall address code_identifier args = imp @@ FP.delegatecall address code_identifier args

    let callthis chk args = imp @@ FP.callthis chk args

    let require b = imp @@ FP.require (FP.return b)

    let proxy address ?caller f =
      match caller with
      | Some caller -> imp @@ FP.proxy address ~caller (unimp f ())
      | None -> imp @@ FP.proxy address (unimp f ())

    let is_admin () = imp @@ FP.is_admin
    let is_admin_caller () = imp @@ FP.is_admin_caller

    let require_admin () = imp @@ FP.require_admin

    let import f = imp @@ FP.import (unimp f ())

    (* Check if a contract has an entry for key `k` *)
    let responds address k = imp @@ FP.responds address k

    let time_get () = imp @@ FP.time_get

    module Echo = struct
      let echo str = imp @@ FP.echo str

      let echo_data k = imp @@ FP.echo_data k

      let echo_pp s = Format.kfprintf (fun _ -> return ()) Format.std_formatter s

      let echo_trace () = imp @@ FP.echo_trace

      let echo_address k = imp @@ FP.echo_address k

      let echo_state () = imp @@ FP.echo_state

      let echo_context () = imp @@ FP.echo_context

      (* Pretty formatting for current (state,context) pair *)
      let echo_env () = imp @@ FP.echo_env
    end
    include Echo

  end

  module Chain = struct

    include Monad

    let tx user address code_identifier args = imp @@ FC.tx user address code_identifier args

    let txr user address code_identifier args =
      imp @@ FC.txr user address code_identifier args

    let tx_create user name t args = imp @@ FC.tx_create user name (unimp t) args

    let tx_proxy user code = imp @@ FC.tx_proxy user (unimp code ())

    let initialize_chain () = env := Nucleus.empty_env

    let time_set i = imp @@ FC.time_set i

    let time_incr i = imp @@ FC.time_incr i

    let state_save name = imp @@ FC.state_save name

    let state_restore name = imp @@ FC.state_restore name

    include Program.Echo
  end
end

