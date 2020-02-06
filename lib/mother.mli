(*include module type of Tradeline_types*)

type tl_id = int

type reduction_command = {
  seller_pos: Tradeline.pos;
  reducer: Tradeline.side;
  clause: Tradeline.clause
}

type t = {
  ledger : Tradeline.Ledger.t;
  tls : (tl_id, Tradeline.t) MP.t;
  max_tl_id : tl_id;
}

val reduce : t -> tl_id -> Tradeline.time -> reduction_command list -> t

val new_tl : t -> Tradeline.addr -> t


