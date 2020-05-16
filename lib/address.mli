type t
[@@deriving show]
(*val pp : Format.formatter -> t -> unit*)

val eq: t -> t -> bool
val next : string -> t
val admin : t
(*val show : t -> string*)
