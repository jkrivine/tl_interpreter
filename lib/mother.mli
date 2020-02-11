(*include module type of Tradeline_types*)

type tl_id

type t
type call

val exec : t -> Tradeline.time -> (Tradeline.addr * call) list -> t


val new_tl : t -> Tradeline.addr -> t


