(* See
 *
https://discuss.ocaml.org/t/avoiding-duplicated-definitions-in-module-implementation-and-interface/1546/4
*)
(*open Base*)
module MP =
  struct
    module M = Base.Map.Poly
    type ('k,'d) t = ('k,'d) M.t
    let empty = M.empty
    let update m k fn = M.update m k ~f:fn
    let find_exn = M.find_exn
    let find = M.find
    let set m k d = M.set m ~key:k ~data:d
    let remove = M.remove
    let singleton = M.singleton
  end

type addr = int (*user/contract address*)
type time = int (*block number*)
type amount = int (*amount of crypto, later a vector of NFTs,FTs,Crypto*)

type pos = int (*Special NFT for positions*)
type side = Seller | Buyer
type testExpr = SellerHas of amount | BuyerHas of amount | Not of testExpr

type clause = {
  t_from: time option; (*None for 0*)
  t_to: time option; (*None for +infty*)
  tests : testExpr list ;
  effect : amount ; (*Pay amount to seller*)
}
type segment = {
  fwd_contract : (pos, clause list) MP.t; (*pos -> c_1 or ... or c_n*)
  bwd_contract : clause list;
}
type tradeline = {
  source: pos; (* First position of the tradeline *)
  (* Invariant: the source cannot be evicted; any position with next=prev=null
   * is evicted, unless they are the source. In that case the tradeline is a
   * singleton *)
  max_pos: pos; (* Source of fresh pos numbers; could be random int *)
  owners: (pos, addr) MP.t;
  next: (pos, pos) MP.t;
  provision : (pos,amount) MP.t; (* positioned provisions*)
  segments : (pos, segment) MP.t; (*[segments.find u] returns the segment between u and u+*)
  (*!!Warning: segments is invariant under backward but is modified by forward.*)
}

type payoff = (addr option * amount) list

exception Throws of string
