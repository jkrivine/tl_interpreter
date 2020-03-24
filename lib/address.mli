type t
val pp : Format.formatter -> t -> unit

val eq: t -> t -> bool
val next : string -> t
val admin : t
val to_string : t -> string
