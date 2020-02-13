(* See
 *
https://discuss.ocaml.org/t/avoiding-duplicated-definitions-in-module-implementation-and-interface/1546/4
*)
open Tools

module SP = Core.Set.Poly

(*open Base*)

type addr = int (*user/contract address*)
type time = int (*block number*)
type amount = int (*amount of crypto, later a vector of NFTs,FTs,Crypto*)
type token = int
type tl_id = int

type pos = int (*Special NFT for positions*)
type side = Seller | Buyer
type testExpr = Higher of token * amount (*>=*)| Lower of token * amount (*<*)
type effectExpr = Pay of token * amount | DrawUpTo of token * amount

type clause = {
  t_from: time option; (*None for 0*)
  t_to: time option; (*None for +infty*)
  tests : testExpr list ;
  effects : effectExpr list;
}

(* Warning: fwd_contract cannot use passive payments ([DrawUpTo]), only active ones ([Pay]) *)
type segment = {
  fwd_contract : (pos, clause list) MP.t; (*pos -> c_1 or ... or c_n*)
  bwd_contract : clause list;
}

type t = {
  id: tl_id;
  source: pos; (* First position of the tradeline *)
  (* Invariant: the source cannot be evicted; any position with next=prev=null
   * is evicted, unless they are the source. In that case the tradeline is a
   * singleton *)
  max_pos: pos; (* Source of fresh pos numbers; could be random int *)
  owners: (pos, addr) MP.t;
  next: (pos, pos) MP.t;
  segments : (pos, segment) MP.t; (*[segments.find u] returns the segment between u and u+*)
  (*!!Warning: segments is invariant under backward but is modified by forward.*)
  dead: pos SP.t;
}

type entry = Eaddr of addr | Epos of tl_id*pos

module Ledger =
struct
  type t = {map : ((entry * token),amount) MP.t ; z_crossings : int}
  let empty = {map = MP.empty ; z_crossings = 0}
  let find_exn l = MP.find_exn l.map
  let find l = MP.find l.map
  let balance l entry tk = MP.find l.map (entry,tk) |? 0
  let add l entry tk a =
    let v = balance l entry tk in
    let z_crossings = if (a+v<0 && v>=0) then l.z_crossings+1
      else if (a+v>=0 && v<0) then l.z_crossings-1 else l.z_crossings in
    {map = MP.set l.map (entry,tk) (a+v) ; z_crossings }
  let transfer l entry1 entry2 tk a = add (add l entry1 tk (-a)) entry2 tk a
  let transferUpTo l entry1 entry2 tk a =
    let a' = min (balance l entry1 tk) a in transfer l entry1 entry2 tk a'
  let transferAll l entry1 entry2 tk =
    let a' = balance l entry1 tk in transfer l entry1 entry2 tk a'
  let solvent l = l.z_crossings = 0
end

exception Throws of string
