module type Monad = sig
  type 'a st
  type 'a unit_st
  type ('a,'b) code_hkey
  type 'a data_hkey

  val bind  : 'a st -> ('a -> 'b st) -> 'b st
  val (>>=) : 'a st -> ('a -> 'b st) -> 'b st
  val ( let* ) : 'a st -> ('a -> 'b st) -> 'b st
  val (>>) : 'a st -> 'b st -> 'b st
  val return : 'a -> 'a st
  val error : string -> 'a st
  val (|?*) : 'a option st -> 'a -> 'a st
end

module type Echo = sig
  type 'a st
  type 'a unit_st
  type ('a,'b) code_hkey
  type 'a data_hkey
(*
 * 'echo' is printing from within the execution
 *)

  (* Echo a string *)
  (* (state,context) arg important for evaluation time of F.p functions *)
  val echo : string -> unit st

  val echo_data : 'a data_hkey -> unit st

  (* Echo the current state *)
  val echo_state : unit unit_st

  (* Echo the current context *)
  val echo_context : unit unit_st

  (* Pretty formatting for current (state,context) pair *)
  val echo_env : unit unit_st
end

module type Program = sig
  include Monad
  (* Initialize a new key for code *)
  val code : unit -> ('a,'b) code_hkey

  val code_set : ('a,'b) code_hkey -> ('a -> 'b st) -> unit st

  val code_private : ('a -> 'b st) -> ('a,'b) code_hkey st


  (* Initialize a new key for data *)
  val data : ?pp:(Format.formatter -> 'a -> unit) -> string -> 'a data_hkey
  (* Initialize a new key which will not show when printing current stat e*)
  (*val data_hidden : ?init:'a -> unit -> 'a data_hkey*)

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
  val require_admin : unit unit_st

  (* Utilities *)
  (* Get context info from inside the execution *)
  val get_caller : Address.t unit_st
  val get_this : Address.t unit_st

  (* Run `f` at a fresh address in a constructor context *)
  val create_contract : string -> ('a -> 'b st) -> 'a -> Address.t st

  (* Run `f` at a fresh address in a constructor context *)
  (* create_user is synonym *)
  val create_empty_contract : string -> Address.t st
  val create_user : string -> Address.t st

  (* Get the code given by `code_hkey` at `address`, run it at `address` *)
  val call : Address.t -> ('a,'b) code_hkey -> 'a -> 'b st

  (* Get the code given by `code_hkey` at `address` but run it in the current context *)
  val delegatecall : Address.t -> ('a,'b) code_hkey -> 'a -> 'b st

  (* Convenience, do a call at current address *)
  val callthis : ('a,'b) code_hkey -> 'a -> 'b st

  (* Check boolean value, throw if false *)
  val require : bool st -> unit st

  (* not natively available in ethereum, included for convenience *)
  val proxy : Address.t -> ?caller:Address.t -> 'a unit_st -> 'a st

  (* Run the given code (only allowed in a constructor, it is used to simulate
     inheritance but `inherit` is a reserved keyword, so we use `import` *)
  val import : 'a unit_st -> 'a st

  (* Check if a contract has an entry for key `k` *)
  val if_responds : Address.t -> ('a,'b) code_hkey -> 'a -> 'b option st

  (* Time stuff, some of it admin-only *)
  val time_get : int unit_st



  include Echo with type 'a st := 'a st
                and type 'a unit_st := 'a unit_st
                and type ('a,'b) code_hkey := ('a,'b) code_hkey
                and type 'a data_hkey := 'a data_hkey
end

module type Chain = sig
  include Monad


  (* User sends a transactions. Ignore return value. Revert to previous state in case of error. *)
  val tx : Address.t -> Address.t -> ('a,'b) code_hkey -> 'a -> unit st

  (* User sends a transactions. Pass along return value. Bubble up an exception in case of error. *)
  val tx_with_return : Address.t -> Address.t -> ('a,'b) code_hkey -> 'a -> 'b st

  (* User sends a contract creation transaction *)
  (* In ethereum, this is just a normal tx but with address 0 as destination *)
  val tx_create : Address.t -> string -> ('a -> 'b st) -> 'a -> Address.t st

  val tx_proxy : Address.t -> 'a unit_st -> 'a st

  val time_set : int -> unit st

  val time_incr : int -> unit st

  (* State saving/restoring, imperative *)
  val state_save : string -> unit st

  val state_restore : string -> unit st

  include Echo with type 'a st := 'a st
                and type 'a unit_st := 'a unit_st
                and type ('a,'b) code_hkey := ('a,'b) code_hkey
                and type 'a data_hkey := 'a data_hkey
end
