(* See
 * https://discuss.ocaml.org/t/avoiding-duplicated-definitions-in-module-implementation-and-interface/1546/4
*)
open Base

type addr = int (*user/contract address*)
and time = int (*block number*)
and amount = int (*amount of crypto*)
and asset = int (*NFT identifier*)

and pos = int (*Special NFT for positions*)

and clause = {
  t_from: time option; (*None for 0*)
  t_to: time option; (*None for +infty*)
  test : testExpr list; (* t_1 and ... and t_k*)
  effect : effectExpr list; (* eff_1 ; ... ; eff_q *)
}


and segment = {
  fwd_contract : (pos, clause list) Map.Poly.t; (*pos -> c_1 or ... or c_n*)
  bwd_contract : clause list;
  ledger : (addr, amount) Map.Poly.t;
}

and tradeline = {
  owners: (pos, addr) Map.Poly.t;
  next: (pos, pos option) Map.Poly.t;
  prev: (pos, pos option) Map.Poly.t;
  underlying : asset option; (*Future: pos (asset list) Map.Poly.t*)
  segments : (pos, segment option) Map.Poly.t; (*[segments.find u] returns the segment between u and u+*)
  (*!!Warning: segments is invariant under backward but is modified by forward.*)
}

and side = Seller | Buyer
and testExpr = LedgerHas of side * amount | TradeLineHas of asset | Not of testExpr
(** [Give a s] Give amount [a] to side [s]*)
and effectExpr = Give of amount * side

and payoff = (addr, amount) Map.Poly.t

exception Throws
