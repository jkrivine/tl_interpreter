(*include module type of Tradeline_types*)

include module type of Tools

type t
type call = REDUCE of pos * pos * side * clause
          | GROW of pos * segment
          | PROVISION of pos * token * amount
          | COLLECT of pos * amount
          | NEW
          (* MAKE_CALL is itself an instruction so unit functions passed as
             arguments can depend on the environment (currently, the caller and current time) *)
          | MAKE_CALL of (addr -> time -> call)


val exec : t -> time -> (addr * call) list -> t

val init_tl : t -> addr -> t

type fwd_contract := (pos, clause list) MP.t

val call_grow_A :
  pos -> fwd_contract -> time -> token -> amount -> time -> call

