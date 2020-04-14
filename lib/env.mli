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
 * Hmap-related stuff
 *)

(*
 * Global chain stuff
 *)

(* Global chain storage *)
(* The global storage is of the form address -> 'a key -> 'a *)
(* Where 'a key is the type of map keys for a data of type 'a *)

type storage

(* Current state contains *)
type state

val empty_state : state

(* An execution environment specifies the current state and calling context *)
type env

val empty_env : env

(*
 * Pseudo-state monad stuff
 *)

(* In-chain commands take an environment as input (ie. a (state,context) pair)
   and return a (value result,state) pair as output *)
type 'a st

val bind  : 'a st -> ('a -> 'b st) -> 'b st
val (>>=) : 'a st -> ('a -> 'b st) -> 'b st
val ( let* ) : 'a st -> ('a -> 'b st) -> 'b st
val (>>) : 'a st -> 'b st -> 'b st
val return : 'a -> 'a st
val error : string -> 'a st
val (|?*) : 'a option st -> 'a -> 'a st


(* Set value associated with [hkey] in this context's hmap *)

(*
 * Code-specific keys
 *)
type ('a,'b) code_hkey

(* Initialize a new key for code *)
val code : unit -> ('a,'b) code_hkey

val code_set : ('a,'b) code_hkey -> ('a -> 'b st) -> unit st

val code_private : ('a -> 'b st) -> ('a,'b) code_hkey st

(* Data *)
(* Code and data keys are treated differently because,
   while data may be updated at any time, we only allow setting the
   code associated with a key (at an address) during the construction of
   the contract *)
(* security-wise, we can add any unset key to hmap of a contract by giving it a
   key with a default value. It shouldn't be able to do anything with it
   though, we just use its data for free *)
type 'a data_hkey

(* Initialize a new key for data *)
val data : ?init:'a -> ?pp:(Format.formatter -> 'a -> unit) -> string -> 'a data_hkey
(* Initialize a new key which will not show when printing current stat e*)
val data_hidden : ?init:'a -> unit -> 'a data_hkey

val data_set : 'a data_hkey -> 'a -> unit st

val data_get : 'a data_hkey -> 'a st

(* Simple read-and-write convenience *)
val data_update : 'a data_hkey -> ('a -> 'a) -> unit st

(* Convenience: define data that will only be visible to
   - constructor methods
   - any inheriting contract if the key is returned by the constructor
*)
val data_private : 'a -> 'a data_hkey st

(*
 * Map-specific convenience functions
 *)

(* Consider [hkey] as some [map]'s name. Set the value of [k] in [map] to [v] *)
val map_set : ('a,'b) MP.t data_hkey -> 'a -> 'b -> unit st

val map_remove : ('a,'b) MP.t data_hkey -> 'a -> unit st

(* Consider [hkey] as some [map]'s name. Get the value of [k] in [map] *)
val map_find : ('a,'b) MP.t data_hkey -> 'a -> 'b option st

val map_find_exns : string -> ('a,'b) MP.t data_hkey -> 'a -> 'b st
val map_find_exn : ('a,'b) MP.t data_hkey -> 'a -> 'b st

exception BadUpdate
val map_update : ('a,'b) MP.t data_hkey -> 'a -> ?default:'b -> ('b -> 'b) -> unit st

(* Admin-only stuff *)
(* Execution at the 'root' is from the admin address.
   Anything nested below that is by a user or a contract
   The admin address can execute some privileged functions *)
val is_admin : unit st

(* Utilities *)
(* Get context info from inside the execution *)
val get_caller : Address.t st
val get_this : Address.t st


(* High-level chain method *)
(* import creates additional keys at the same address *)
(* internal call is just calling the method. otherwise do call or delegatcall *)

(* Run an instruction sequence on the empty environment *)
val execute : 'a st -> 'a option

(* Run `f` at a fresh address in a constructor context *)
val create_contract : string -> 'a st -> (Address.t * 'a) st

(* Create a new user address. No code, no data. *)
val create_user : string -> Address.t st

(* Get the code given by `code_hkey` at `address`, run it at `address` *)
val call : Address.t -> ('a,'b) code_hkey -> 'a -> 'b st

(* Get the code given by `code_hkey` at `address` but run it in the current context *)
val delegatecall : Address.t -> ('a,'b) code_hkey -> 'a -> 'b st

(* Convenience, do a call at current address *)
val callthis : ('a,'b) code_hkey -> 'a -> 'b st

(* Check if result of evaluating two commands, are equal *)
val is_equal : 'a st -> 'a st -> bool st

(* Check boolean value, throw if false *)
val require : bool st -> unit st


(* Run the given code (only allowed in a constructor, it is used to simulate
   inheritance but `inherit` is a reserved keyword, so we use `import` *)
val import : 'a st -> 'a st

(* User sends a transactions *)
val tx : Address.t -> Address.t -> ('a,'b) code_hkey -> 'a -> unit st

(* User sends a contract creation transaction *)
(* In ethereum, this is just a normal tx but with address 0 as destination *)
val tx_create : Address.t -> string -> 'a st -> Address.t st

(* not natively available in ethereum, included for convenience *)
val proxy : Address.t -> ?caller:Address.t -> 'a st -> 'a st

(* Check if a contract has an entry for key `k` *)
val if_responds : Address.t -> ('a,'b) code_hkey -> 'a -> 'b option st

(* Time stuff, some of it admin-only *)
val time_get : int st

val time_set : int -> unit st

val time_incr : int -> unit st

(* State saving/restoring, imperative *)
type history
val history : history

val state_save : string -> unit st

val state_restore : string -> unit st

(*
 * 'echo' is printing from within the execution
 *)

(* Echo a string *)
(* (state,context) arg important for evaluation time of F.p functions *)
val echo : string -> unit st

(* Warning: will pretend data is initialized to default value given any key
   with an `init` field, even if the key is actually unset at the current
   address *)
val echo_data : 'a data_hkey -> unit st

(* Echo the current state *)
val echo_state : unit st

(* Echo the current context *)
val echo_context : unit st

(* Pretty formatting for current (state,context) pair *)
val echo_env : unit st
