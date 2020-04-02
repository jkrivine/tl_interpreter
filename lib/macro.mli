open Env
module A = Address

val zwrap : A.t -> ('a, 'b) code_hkey -> (A.t * 'a -> 'b st) -> unit st
