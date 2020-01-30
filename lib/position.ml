open Base

type addr = int (*user/contract address*)
type time = int (*block number*)
type amount = int (*amount of crypto*)
type asset = int (*NFT identifier*)

type pos = int (*Special NFT for positions*)

type segment = {
  fwd_contract : pos (clause list) Map.Poly.t; (*pos -> c_1 or ... or c_n*)
  bwd_contract : clause list;
  ledger : addr amount Map.Poly.t;
}

type tradeline = {
  owners: pos addr Map.Poly.t;
  next: pos (pos option) Map.Poly.t;
  prev: pos (pos option) Map.Poly.t;
  underlying : asset option (*Future: pos (asset list) Map.Poly.t*)
  segments : pos (segment option) Map.Poly.t (*[segments.find u] returns the segment between u and u+*)
  (*!!Warning: segments is invariant under backward but is modified by forward.*)
}

type side = Seller | Buyer
type testExpr = LedgerHas of side * amount | TradeLineHas of asset | Not of testExpr

(** [Give a s] Give amount [a] to side [s]*)
type effectExpr = Give of amount * side

type clause = {
  t_from: time option; (*None for 0*)
  t_to: time option; (*None for +infty*)
  test : testExpr list; (* t_1 and ... and t_k*)
  effect : effectExpr list; (* eff_1 ; ... ; eff_q *)
}

(**[transfer tl p player] transfer ownership of position [p] to [player]*)
val transfer : tradeline -> pos -> addr -> tradeline

(**[provision tl p player a] provisions [a] coins to segment after position [p] for [player], NB(ext) anyone can call*)
val provision : tradeline -> pos -> addr -> amount -> tradeline

type payoff = addr amount Map.Poly.t

exception Throws
(**[reduce tl p Seller t c] Backward reduction of [p] at time [t] on clause [c]. NB(ext) caller should be owner of [p]*)
(**[reduce tl p Buyer t c] Forward reduction of [p+] at time [t] on clause [c]. NB(ext) caller should be owner of [p+]*)
val reduce : tradeline -> pos -> side -> time -> clause -> (tradeline * payoff)

(*val gc:
val grow:
val bind:
val unbind:
*)



