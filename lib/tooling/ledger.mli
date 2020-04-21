open Imperative.P
(** Syntactic sugar *)
module A = Address

type token = A.t
[@@deriving show]

type pos = A.t
[@@deriving show]

type amount = int

type t

val add : t data_hkey -> A.t -> ?index:string -> token -> amount -> unit
val balance : t data_hkey -> A.t -> ?index:string -> token -> amount
val transfer : t data_hkey -> A.t -> ?indices:(string*string) -> token -> amount -> A.t -> unit
val transfer_up_to : t data_hkey -> A.t -> ?indices:(string*string) -> token -> amount -> A.t -> unit
val transfer_all : t data_hkey -> A.t -> ?indices:(string*string) -> token -> A.t -> unit
val solvent : t data_hkey -> bool
val pp : Format.formatter -> t -> unit
val empty : t
val zwrap_start : t data_hkey -> unit
val zwrap_end : t data_hkey -> unit
val is_zwrapping : t data_hkey -> bool

