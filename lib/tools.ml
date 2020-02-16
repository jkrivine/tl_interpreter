module SP = Core.Set.Poly

let (|?) a default = Option.value a ~default

type token = int
type addr = int
type time = int
type amount = int
type tl_id = int (* tradeline id *)
type pos = int (*Special NFT for positions*)
type side = Seller | Buyer
type testExpr =
    (*OracleHte of addr * amount*)
  | ProvisionHte of token * amount (*higher than or equal*)
  (*| OracleLt of addr * amount*)
  | ProvisionLt of token * amount (*lower than*)
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

type entry = Eaddr of addr | Epos of pos

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
