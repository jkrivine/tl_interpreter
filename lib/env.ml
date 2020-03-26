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

type storage = (Address.t,HM.t) MP.t

(* Current state contains *)
type state = {
  (* Global storage *)
  hmaps: storage;
  (* Current block number *)
  time: int;
}

let empty_state = {
  hmaps = MP.empty;
  time = 0
}


let pp_state fmt state =
  F.p fmt "State at time %d:" state.time;
  F.cr ();
  MP.pp Address.pp HM.pp fmt state.hmaps;
  F.p fmt "\n"

(* Type of an execution context *)
type context = {
  caller: Address.t;
  this: Address.t;
  (* Assigning code is only allowed during creation of a new contract *)
  constructor: bool
}

let  empty_context = {
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
   else ());

(* An execution environment specifies the current state and calling context *)
type env = state*context

let empty_env =
  (empty_state,empty_context)

(*
 * Pseudo-state monad stuff
 *)

(* In-chain commands take an environment as input (ie. a (state,context) pair)
   and return a (value result,state) pair as output *)
type 'a st = env -> ( ('a,string) result * state )

let _is_admin context = context.this = Address.admin

let bind t1 t2 =
  fun (s,c) -> match t1 (s,c) with
    | (Ok v,s') -> t2 v (s',c)
    | (Error v,s') -> (Error v,s')
let (>>=) = bind
let ( let* ) t1 t2 = bind t1 t2
let (>>) t1 t2 = bind t1 (fun _ -> t2)
(* Not the regular, argument-forgetting (>>). We reuse it as a generic exception handler,
    only at the 'admin' level of execution. *)
(* Not using it because left-associativity of (>>) makes it not useful *)
(*let (>>) t1 t2 = fun (s,c) -> match t1 (s,c) with*)
  (*| (Ok _,s') -> t2 (s',c)*)
  (*| (Error v,s') ->*)
    (*if _is_admin c then begin*)
      (*F.p Format.std_formatter "Error: %s" v;*)
      (*F.cr ();*)
      (*F.p Format.std_formatter "Reverting state...";*)
      (*F.cr ();*)
      (*t2 (s,c)*)
      (*end else (Error v,s')*)

let return v = fun (s,_) -> (Ok v,s)
let error v = fun (s,_) -> (Error v,s)
let (|?*) a default = a >>= function Some e -> return e | None -> return default

(* Admin-only stuff *)
(* Execution at the 'root' is from the admin address.
   Anything nested below that is by a user or a contract
   The admin address can execute some privileged functions *)
let ite_admin t1 t2 (state,context) =
  (if _is_admin context then t1 () else t2 ()) (state,context)


let is_admin =
  ite_admin (fun () -> return ()) (fun () -> error "not admin")

(*
 * Low-level read/write in global storage
 *)

(* Get the hmap associated with the current address *)
(* Creates hmap if needed *)
(* Direct access version *)
let _get_hmap address hmaps = match MP.find hmaps address with
  | Some hmap -> hmap
  | None -> HM.empty
(* Monadic version *)
let get_hmap (state,context) =
  (Ok (_get_hmap context.this state.hmaps),state)

(* Set the hmap associated with the current address *)
let set_hmap hmap ({hmaps; _} as state,context) =
  let hmaps' = MP.set hmaps context.this hmap in
  (Ok (),{state with hmaps=hmaps'})


(* Get value associated with [hkey] in this context's hmap *)
(* Direct access version *)
let _get_in_hmap_option hkey address hmaps = HM.find hkey (_get_hmap address hmaps)
(* Monadic version *)
let get_in_hmap_option hkey (state,context) =
  (Ok (_get_in_hmap_option hkey context.this state.hmaps), state)

(* Get value associated with [hkey] in this context's hmap *)
let get_in_hmap hkey =
  get_in_hmap_option hkey >>= function
  | Some v -> return v
  | None -> error "No such hkey"

(* Set value associated with [hkey] in this context's hmap *)
let set_in_hmap hkey v =
  get_hmap >>= fun hmap ->
  let hmap' = HM.add hkey v hmap in
  set_hmap hmap'

(*
 * Code-specific keys
 *)
type ('a,'b) code_hkey = ('a -> 'b st) HM.key

(* Initialize a new key for code *)
let code () = HM.Key.create {name="<code>";init=None;pp=None}

let code_set code_hkey code (state,context) =
  if context.constructor then
    (get_in_hmap_option code_hkey >>= function
    | Some _ -> error "Code already set at this hkey"
    | None -> set_in_hmap code_hkey code ) (state,context)
  else
    (Error "Cannot set code outside of constructor", state)

let code_private f (state,context) =
  if context.constructor then
    (let k = code () in code_set k f >> return k) (state,context)
  else
    (Error "Cannot declare private code outside of constructor", state)

(* Data *)
(* Code and data keys are treated differently because,
   while data may be updated at any time, we only allow setting the
   code associated with a key (at an address) during the construction of
   the contract *)
(* security-wise, we can add any unset key to hmap of a contract by giving it a
   key with a default value. It shouldn't be able to do anything with it
   though, we just use its data for free *)
type 'a data_hkey = 'a HM.key

(* Initialize a new key for data *)
let data ?init ?pp name = HM.Key.create {name;init;pp}

let data_set (data_hkey: 'a data_hkey) (v:'a) = set_in_hmap data_hkey v

let data_get data_hkey = match HM.Key.info data_hkey with
  | {init=Some default;_} -> get_in_hmap_option data_hkey >>= (function
    | Some v -> return v
    | None -> return default)
  | {init=None;_} -> get_in_hmap data_hkey

(* Simple read-and-write convenience *)
let data_update data_hkey f =
  let* v = data_get data_hkey in
  set_in_hmap data_hkey (f v)

(* Convenience: define data that will only be visible to
   - constructor methods
   - any inheriting contract if the key is returned by the constructor
*)
let data_private v (state,context) =
  if context.constructor then
    (return (data ~init:v "<private>")) (state,context)
  else
    (error "Cannot declare private data outside of constructor") (state,context)


(*
 * Map-specific convenience functions
 *)

(* Consider [hkey] as some [map]'s name. Set the value of [k] in [map] to [v] *)
let map_set data_hkey k v =
  data_update data_hkey (fun m -> MP.set m k v)

let map_remove data_hkey k =
  data_update data_hkey (fun m -> MP.remove m k)

(* Consider [hkey] as some [map]'s name. Get the value of [k] in [map] *)
let map_find data_hkey k =
  data_get data_hkey >>= fun map -> return (MP.find map k)

let map_find_exn data_hkey k =
  map_find data_hkey k >>= function Some v -> return v | None -> error "Not found"

exception BadUpdate
let map_update data_hkey k ?default f =
  try
  data_update data_hkey (fun m ->
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


(* High-level chain method *)
(* import creates additional keys at the same address *)
(* internal call is just calling the method. otherwise do call or delegatcall *)

(* Run an instruction sequence on the empty environment *)
let execute f =
  match f empty_env with
  | (Ok v,_) -> Some v
  | (Error _,_) -> None

(* Run `f` at a fresh address in a constructor context *)
let create_contract name f (state,context) =
  let address = Address.next name in
  let hmaps' = MP.set state.hmaps address HM.empty in
  let state' = {state with hmaps=hmaps'} in
  let context' = {constructor=true;this=address;caller=context.this} in
  (f >>= fun ret -> return (address,ret)) (state',context')

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
  let context' = {constructor=true;this=address;caller=context.this} in
  ((C.construct args) >>= fun ret -> return (address,ret)) (state',context')
[@@ocaml.warning "-32"]

(* Create a new user address. No code, no data. *)
let create_user name (state,context) =
  (create_contract name (return ()) >>= fun (a,()) -> return a) (state,context)

(* Get the code given by `code_hkey` at `address`, run it in *context'` instead
   of the current context *)
let _call context' address code_hkey args (state,_) =
  match _get_in_hmap_option code_hkey address state.hmaps with
  | Some code -> code args (state,context')
  | None -> (Error "Code not found",state)

(* Get the code given by `code_hkey` at `address`, run it at `address` *)
let call address code_hkey args (state,context) =
  let context' = {constructor=false; this=address;caller=context.this} in
  _call context' address code_hkey args (state,context)

(* Get the code given by `code_hkey` at `address` but run it in the current context *)
let delegatecall address code_hkey args (state,context) =
  _call context address code_hkey args (state,context)

(* Convenience, do a call at current address *)
let callthis chk args (state,context) =
  delegatecall context.this chk args (state,context)


(* Run the given code (only allowed in a constructor, it is used to simulate
   inheritance but `inherit` is a reserved keyword, so we use `import` *)
let import f (state,context) =
  if context.constructor = true
  then f (state,context)
  else (Error "cannot inherit outside of constructor",state)

(* User sends a transactions *)
let tx user address code_hkey args =
  is_admin >> fun (old_state,context) ->
  let final_state = match (call address code_hkey args) (old_state,{context with this=user}) with
  | (Ok _,new_state) -> new_state
  | (Error v,_) ->
      F.p Format.std_formatter "Error: %s" v;
      F.cr ();
      F.p Format.std_formatter "Reverting state...";
      F.cr ();
      old_state in
  (Ok (),final_state)

(* User sends a contract creation transaction *)
(* In ethereum, this is just a normal tx but with address 0 as destination *)
let rethrow t (state,context) = match t (state,context) with
  | (Ok _,_) as r -> r
  | (Error v,_) -> failwith v

let tx_create user name t =
  is_admin >> fun (state,context) ->
  (rethrow (create_contract name t) >>= fun (a,_) -> return a) (state,{context with this=user})

(* not natively available in ethereum, included for convenience *)
let proxy address f =
  is_admin >> fun (state,context) ->
  f (state,{context with this=address})

(* Check if a contract has an entry for key `k` *)
let if_responds address (k:('a,'b) code_hkey) (args:'a) : 'b option st = fun (state,context) ->
   (match _get_in_hmap_option k address state.hmaps with
  | Some c -> c args >>= fun ret -> return (Some ret)
  | None -> return None) (state,context)

(* Time stuff, some of it admin-only *)
let time_get (state,_) =
  (Ok state.time,state)

let time_set i =
    is_admin >> (fun (state,_context) ->
  (Ok (),{state with time=i}))

let time_incr i =
  is_admin >> fun (state,_context) ->
  (Ok (),{state with time=state.time+i})


(* State saving/restoring *)
type history = (string,state) MP.t ref
let history = ref MP.empty

let state_save name =
  is_admin >> fun (state,context) ->
  (return (history := MP.set !history name state)) (state,context)

let state_restore name =
  is_admin >> fun _env ->
  let state' = MP.find_exn !history name in
  (Ok (),state')

(*
 * 'echo' is printing from within the execution
 *)

(* Echo a string *)
(* (state,context) arg important for evaluation time of F.p functions *)
let echo str (state,context) =
  (ite_admin
  (fun () -> return (F.p Format.std_formatter "%s" str;F.cr ()))
  (fun () -> return (F.p Format.std_formatter "%a ⇒ " Address.pp context.this;
           F.p Format.std_formatter "%s" str;F.cr ())))
  (state,context)

(* Warning: will pretend data is initialized to default value given any key
   with an `init` field, even if the key is actually unset at the current
   address *)
let echo_data k =
  let* v = data_get k in
  HM.pp_binding Format.str_formatter (HM.B (k,v));
  echo (Buffer.contents Format.stdbuf)

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
