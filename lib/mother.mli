(*include module type of Tradeline_types*)

module T := Tradeline

type t
type call = REDUCE of T.tl_id * T.pos * T.pos * T.side * T.clause
          | GROW of T.tl_id * T.pos * T.segment
          | PROVISION of T.tl_id * T.pos * T.token * T.amount
          | COLLECT of T.tl_id * T.pos * T.amount
          | NEW
          (* MAKE_CALL is itself an instruction so unit functions passed as
             arguments can depend on the environment (currently, the caller and current time) *)
          | MAKE_CALL of (T.addr -> T.time -> call)


val exec : t -> T.time -> (T.addr * call) list -> t

val new_tl : t -> T.addr -> t

type fwd_contract := (T.pos, T.clause list) MP.t

val call_grow_A :
  T.tl_id -> T.pos -> fwd_contract -> T.time -> T.token -> T.amount -> T.time -> call

