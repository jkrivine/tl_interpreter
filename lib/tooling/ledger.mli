(** Dec's ledger has its own module *)
open Env.Imp.Program

type address := Address.t

type token = address

type pos = address

type amount = int

(** Contains all the token balances as well as the current zcrossing state. *)
type t

(** Ledger's function operate on a key for [t] (that is, a [t data_id]). *)
type t_id := t data_id

(** Fresh empty ledger *)
val empty : t

(** {1 Transfers and balances} *)

(** Each optional [~index] argument is a named compartment to further segregate funds. By default, it is [""]. *)

(** Credit [address] with [amout] [token]s. *)
val add : t_id -> address -> ?index:string -> token -> amount -> unit

(** Check current balance of [address] *)
val balance : t_id -> address -> ?index:string -> token -> amount

(** Internal token transfer *)
val transfer : t_id -> address -> ?indices:(string*string) -> token -> amount -> address -> unit

(** Internal token transfer, do not attempt to transfer more than giver balance. *)
val transfer_up_to : t_id -> address -> ?indices:(string*string) -> token -> amount -> address -> unit

(** Convenience: transfer all of some token balance *)
val transfer_all : t_id -> address -> ?indices:(string*string) -> token -> address -> unit

(** {1 Zwrapping} *)

(** By default all transfers check for [solvent] and raise if not solvent. The following methods let one ignore insolvency for a while. *)

(** Check if Ledger currently owes money to one of its user (i.e. some user has a negative balance). A transaction must always revert or find a way to fix the negative balance. *)
val solvent : t_id -> bool

val zwrap_start : t_id -> unit
val zwrap_end : t_id -> unit
val is_zwrapping : t_id -> bool

(** {1 Printing} *)

(* Low-level pretty-printing function *)
val pp_custom : Format.formatter -> t -> (Format.formatter -> pos -> string -> pos -> amount -> unit) -> unit
val pp : Format.formatter -> t -> unit


(** {1 Utility methods with unbounded gas consumption} *)
module Offchain : sig
  val iter_address_i : t_id -> address -> ?index:string -> (token -> int -> unit) -> unit
  val restrict : t_id -> (address -> ((string*token),amount) MP.t -> bool) -> unit
  val update : t_id -> ((address * string * token) -> amount -> amount) -> unit
end
