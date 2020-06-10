(** This is an interface file. It provides the low-level APIs for writing smart contracts and interacting with the chain. *)

(** The API is divided into [Program], which contains Solidity-like facilities,
    and [Chain], which is about submitting transactions to the chain. They
    share [Monad] and [Echo] as common interfaces.  *)

(** Some of the complexity comes from the existence of two parallel implementations.
    One is functional and relies on a state+exception monad. The other is imperative.
    It directly modifies a state reference and uses OCaml exceptions.
    The Imperative is implemented using the Functional. *)

(** Generic state+exception monad *)
module type Monad = sig
  (** A command to be executed in a chain environment 
      may return a value of type ['a]. *)
  type 'a st

  (** When a command [c] has no argument, it should be used as [c] in the
      functional implementation, but as [c ()] in the imperative one (since
      OCaml evaluates eagerly). *)
  type 'a unit_st

  (** An identifier for a stored function that takes inputs of type ['a] and outputs values of type ['b].
      Akin to the first 4 bytes of the Keccak256 of a function signature in Solidity (except a function signature in Solidity does not specify return type).
      Note that in Ethereum, method [fn(int a)] stored at address [0x1] may be completely different from [fn(int a)] stored at [0x2]. 
      Similarly, for a given key [k : ('a,'b) code_id], different addresses may give different implementations of [k]. *)
  type ('a,'b) code_id

  (** An identifier for a value. Similar to a storage variable in Solidity, except here it's just a key.
      The difference with a [code_id] is that functions stored as
      values cannot be executed, but can be rewritten (once a [code_id]
      is set at an address, it cannot be changed). *)
  type 'a data_id

  (** [bind st (fun v -> st')] chains command [st] and [st'], allowing [st'] to
      depend on the output of [st]. Note that in the imperative version, [bind
      t1 t2 = t2 t1]. *)
  val bind  : 'a st -> ('a -> 'b st) -> 'b st

  (** Syntactic sugar for [bind] *)
  val (>>=) : 'a st -> ('a -> 'b st) -> 'b st

  (** Also sugar for [bind] *)
  val ( let* ) : 'a st -> ('a -> 'b st) -> 'b st

  (** Syntactic sugar for [bind] with argument ignored *)
  val (>>) : 'a st -> 'b st -> 'b st

  (** [return v] wraps [v] in a chain command that immediately returns [v] upon
      execution. *)
  val return : 'a -> 'a st

  (** Raises an error. Can be another constructor of the state+exception monad
      (in the functional implementation), or an actual OCaml exception (in the
      imperative implementation). *)
  val error : string -> 'a st

  (** Syntactic sugar. Same as [(|?)] for ['a option], but lifted to the ['a
      option st] type. *)
  val (|?*) : 'a option st -> 'a -> 'a st
end

(** On-chain logging module *)
module type Echo = sig

  (** {1 Base functionality} *)

  type 'a st
  type 'a unit_st
  type ('a,'b) code_id
  type 'a data_id

  (** {1 Logging} *)

  (** Echo a string
      (state,context) arg important for evaluation time of F.p functions *)
  val echo : string -> unit st

  (** In the current context, log the data associated to the argument *)
  val echo_data : 'a data_id -> unit st
      
  (** Generic logging *)
  val echo_pp : ('a, Format.formatter, unit, unit st) format4 -> 'a

  (** Log the current address *)
  val echo_address : Address.t -> unit st

  (** Display of trace at the current code point *)
  val echo_trace : unit unit_st

  (** Echo the current state *)
  val echo_state : unit unit_st

  (** Echo the current context *)
  val echo_context : unit unit_st

  (** Pretty formatting for current (state,context) pair *)
  val echo_env : unit unit_st
end

(** API for coding smart contracts *)
module type Program = sig

  (** {1 Base functionality} *)

  include Monad

  (** {1 Stored code} *)

  (** Initialize a new key for code.
      If not part of a module or returned from a constructor,
      the key will be private. So even proxying will be impossible.
      If you want proxying to be impossible but still need the key to be accessible,
      set [~internal:true].
  *)
  val code : ?internal:bool -> unit -> ('a,'b) code_id

  (** Associate chain code to a code identifier. May only be used in a constructor *)
  val code_set : ('a,'b) code_id -> ('a -> 'b st) -> unit st

  (** Return a fresh constructor with associated code. Same as using [code] with a local variable. *)
  val code_private : ('a -> 'b st) -> ('a,'b) code_id st

  (** {1 Stored data} *)

  (** Initialize a new key for data.
      [~pp] may specify a formatter. 
      More simply, [~show] may specify a printing function ([~pp] has priority).
      The [string] argument will be used as the name for the data in logging facilities.
  *)
  val data : ?pp:(Format.formatter -> 'a -> unit) -> ?show:('a -> string) -> string -> 'a data_id

  (** Initialize a new key which will not show when printing current state. *)
  val data_hidden : unit -> 'a data_id

  (** Same as assigning a storage variable in Solidity. *)
  val data_set : 'a data_id -> 'a -> unit st

  (** Same as reading a storage variable. *)
  val data_get : 'a data_id -> 'a st

  (** Convenience: [data_update k (fun v -> v')] sets the data associated with [k] to [v'] *)
  val data_update : 'a data_id -> ('a -> 'a) -> unit st

  (** Convenience: define data that will only be visible to
     - constructor methods
     - any inheriting contract if the key is returned by the constructor

  *)
  val data_private : 'a -> 'a data_id st

  (** {2 Map-specific convenience functions} *)

  (** In [map_set identifier k v], consider [identifier] as the name of some [map]. Set the value of [k] in [map] to [v] *)
  val map_set : ('a,'b) MP.t data_id -> 'a -> 'b -> unit st

  (** [map_remove identifier k] deletes the value associated to k *)
  val map_remove : ('a,'b) MP.t data_id -> 'a -> unit st

  (** Consider [identifier] as some [map]'s name. Get the value of [k] in [map] *)
  val map_find : ('a,'b) MP.t data_id -> 'a -> 'b option st

  (** Throws an exception with helpful [string] *)
  val map_find_exns : string -> ('a,'b) MP.t data_id -> 'a -> 'b st

  (** Throws a generic exceptoin *)
  val map_find_exn : ('a,'b) MP.t data_id -> 'a -> 'b st

  exception BadUpdate

  (** Convenience update function *)
  val map_update : ('a,'b) MP.t data_id -> 'a -> ?default:'b -> ('b -> 'b) -> unit st

  (** {1 Admin-only stuff} *)

  (** Execution at the 'root' is from the admin address.
     Anything nested below that is by a user or a contract
     The admin address can execute some privileged functions *)

  (** [true] iff current address is [Address.admin] *)
  val is_admin : bool unit_st

  (** [true] iff current caller is [Address.admin] *)
  val is_admin_caller : bool unit_st

  (** Raises exception unless [is_admin] *)
  val require_admin : unit unit_st

  (** {1 Creating contracts} *)

  (** Run [f] at a fresh address in a constructor context *)
  val create_contract : string -> ('a -> 'b st) -> 'a -> Address.t st

  (** Run [f] at a fresh address in a constructor context *)
  val create_empty_contract : string -> Address.t st

  (** [create_user] is synonym *)
  val create_user : string -> Address.t st

  (** {1 Calling contract code} *)

  (** Get the code given by [code_id] at [address], run it at [address] *)
  val call : Address.t -> ('a,'b) code_id -> 'a -> 'b st

  (** Get the code given by [code_id] at [address] but run it in the current context *)
  val delegatecall : Address.t -> ('a,'b) code_id -> 'a -> 'b st

  (** Convenience, do a call at current address *)
  val callthis : ('a,'b) code_id -> 'a -> 'b st

  (** {1 Misc. } *)

  (** Get current caller *)
  val get_caller : Address.t unit_st

  (** Get current address *)
  val get_this : Address.t unit_st

  (** Check boolean value, throw if false *)
  val require : bool st -> unit st

  (** not natively available in ethereum, included for convenience *)
  val proxy : Address.t -> ?caller:Address.t -> 'a unit_st -> 'a st

  (** Run the given code (only allowed in a constructor, it is used to simulate
     inheritance but [inherit] is a reserved keyword, so we use [import] *)
  val import : 'a unit_st -> 'a st

  (** Check if a contract has an entry for key [k] *)
  val responds : Address.t -> ('a,'b) code_id -> bool st

  (** Time stuff, some of it admin-only *)
  val time_get : int unit_st

  (** {1 Logging} *)

  include Echo with type 'a st := 'a st
                and type 'a unit_st := 'a unit_st
                and type ('a,'b) code_id := ('a,'b) code_id
                and type 'a data_id := 'a data_id
end

(** API for interacting with the chain from the outside *)
module type Chain = sig

  (** {1 Base functionality} *)

  include Monad

  (** {1 Transactions} *)

  (** User sends a transactions. Ignore return value. Revert to previous state in case of error. *)
  val tx : Address.t -> Address.t -> ('a,'b) code_id -> 'a -> unit st

  (** User sends a transactions. Pass along return value. Bubble up an exception in case of error. *)
  val txr : Address.t -> Address.t -> ('a,'b) code_id -> 'a -> 'b st

  (** User sends a contract creation transaction *)
  (** In ethereum, this is just a normal tx but with address 0 as destination *)
  val tx_create : Address.t -> string -> ('a -> 'b st) -> 'a -> Address.t st

  (** [tx_proxy a c] runs commands [c] as a single transaction, originating
      from [a]. Useful for executing arbitrary code from the [Chain] interface.
  *)
  val tx_proxy : Address.t -> 'a unit_st -> 'a st

  (** {1 Time/state manipulation} *)

  (** Change current block number. Cannot go back. *)
  val time_set : int -> unit st

  val time_incr : int -> unit st

  (** State saving/restoring, imperative *)
  val state_save : string -> unit st

  val state_restore : string -> unit st

  (** {1 Logging} *)

  include Echo with type 'a st := 'a st
                and type 'a unit_st := 'a unit_st
                and type ('a,'b) code_id := ('a,'b) code_id
                and type 'a data_id := 'a data_id
end
