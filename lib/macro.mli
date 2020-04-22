open Imperative.P
module A = Address

val zwrap : A.t -> ('a, 'b) code_identifier -> (A.t * 'a -> 'b) -> unit
