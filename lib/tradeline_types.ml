(* See
 *
https://discuss.ocaml.org/t/avoiding-duplicated-definitions-in-module-implementation-and-interface/1546/4
*)
(*open Base*)
module MP = Base.Map.Poly

type addr = int (*user/contract address*)
and time = int (*block number*)
and amount = int (*amount of crypto*)
and asset = int (*NFT identifier*)

and pos = int (*Special NFT for positions*)

and clause = {
  t_from: time option; (*None for 0*)
  t_to: time option; (*None for +infty*)
  tests : testExpr list; (* t_1 and ... and t_k*)
  effects : effectExpr list; (* eff_1 ; ... ; eff_q *)
}


and segment = {
  fwd_contract : (pos, clause list) MP.t; (*pos -> c_1 or ... or c_n*)
  bwd_contract : clause list;
  ledger : (addr, amount) MP.t;
}

and tradeline = {
  source: pos; (* First position of the tradeline *)
  (* Invariant: the source cannot be evicted; any position with next=prev=null
   * is evicted, unless they are the source. In that case the tradeline is a
   * singleton *)
  max_pos: pos; (* Source of fresh pos numbers; could be random int *)
  owners: (pos, addr) MP.t;
  next: (pos, pos) MP.t;
  prev: (pos, pos) MP.t;
  underlying : asset option; (*Future: pos (asset list) MP.t*)
  segments : (pos, segment) MP.t; (*[segments.find u] returns the segment between u and u+*)
  (*!!Warning: segments is invariant under backward but is modified by forward.*)
}

and side = Seller | Buyer
and testExpr = LedgerHas of amount * side | TradeLineHas of asset | Not of testExpr
(** [Give a s] Give amount [a] to side [s]*)
and effectExpr = Give of amount * side

and payoff = (addr, amount) MP.t

exception Throws of string
