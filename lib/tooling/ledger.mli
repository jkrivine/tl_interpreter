open Env.Imp.Program
(** Syntactic sugar *)
module A = Address

type token = A.t
[@@deriving show]

type pos = A.t
[@@deriving show]

type amount = int

type t

val add : t data_identifier -> A.t -> ?index:string -> token -> amount -> unit
val balance : t data_identifier -> A.t -> ?index:string -> token -> amount
val transfer : t data_identifier -> A.t -> ?indices:(string*string) -> token -> amount -> A.t -> unit
val transfer_up_to : t data_identifier -> A.t -> ?indices:(string*string) -> token -> amount -> A.t -> unit
val transfer_all : t data_identifier -> A.t -> ?indices:(string*string) -> token -> A.t -> unit
val solvent : t data_identifier -> bool
val pp_custom : Format.formatter -> t -> (Format.formatter -> pos -> string -> pos -> amount -> unit) -> unit
val pp : Format.formatter -> t -> unit
val empty : t
val zwrap_start : t data_identifier -> unit
val zwrap_end : t data_identifier -> unit
val is_zwrapping : t data_identifier -> bool


module Offchain : sig
  val iter_address_i : t data_identifier -> A.t -> ?index:string -> (token -> int -> unit) -> unit
  val restrict : t data_identifier -> (A.t -> ((string*token),amount) MP.t -> bool) -> unit
  val update : t data_identifier -> ((A.t * string * token) -> amount -> amount) -> unit
end
