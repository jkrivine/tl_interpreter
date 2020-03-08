open Tools


(** The following types are all int for convenience. They should more concretely be addresses, block numbers or random identifiers *)

type token = int
[@@deriving show]

type addr = int
[@@deriving show]

type time = int
[@@deriving show]

type amount = int
[@@deriving show]

type tl_id = int (* tradeline id *)
[@@deriving show]

type pos = int (*Special NFT for positions*)
[@@deriving show]

(** Generic representation for choosing which side of a tradeline segment we're talking about. *)
type side = Seller | Buyer
[@@deriving show]

(** A clause runs after checking that its associated [testExprs] have all been validated.
    Current tests only check whether a position has a low of high enough balance. *)
type testExpr =
    (*OracleHte of addr * amount*)
  | ProvisionHte of token * amount
  (*| OracleLt of addr * amount*)
  | ProvisionLt of token * amount (*lower than*)
  | HasPaidLt of token * amount (* Check notarized payments *)
  | SellerIs of pos (* Look at current position on seller side *)
  (*| ReadOracle of oracle (* Read a value *)*)
  (*| IsLocked of lock (* Read a lock *)*)
[@@deriving show]

(** A clause effects can either be payment (from the caller to its counterpart in the local tradeline segment) or a provision transfer (from the caller's counterpart's position to the caller). *)
and effectExpr =
  | Pay of token * amount
  | DrawUpTo of token * amount
  | Reduce
  | Proxy of call
  (*| SetLock of lock * bool (* Set a lock *)*)
[@@deriving show]

and call =
  | TRIGGER of pos * pos * side * clause
  | GROW of pos * segment
  | PROVISION of pos * token * amount
  | COLLECT_TOKEN of pos * token
  | COLLECT_POS of pos * pos
  | NEW
  | MAKE_CALL of (entry -> time -> call)
  (*| INIT_LOCK of pos * lock * bool*)
[@@deriving show]

(** A clause is a possible choice for how to resolve a contract. It specifies when it is applicable, a list of tests and a list of effects *)
and clause = {
  t_from: time;
  t_to: time option; (** None for +infty*)
  tests : testExpr list ;
  effects : effectExpr list;
}
[@@deriving show]

(** A segment connects two positions: the seller side and the buyer side. The forward contract specifies how the buyer side can acquire the seller side; it should map from all positions in the buyer's past to a list of clauses (TODO). The backward contract spceifies how the seller side can kick out the seller.

  Warning: fwd_contract cannot use passive payments ([DrawUpTo]), only active ones ([Pay]) *)
and segment = {
  fwd_contract : clause list;
  (*pos -> c_1 or ... or c_n*)
  bwd_contract : clause list;
}
[@@deriving show]

(** Ledger entries can represent either users or positions *)
and entry = Eaddr of addr | Epos of pos
[@@deriving show]

module Ledger =
struct
  type t = {map : ((entry * token),amount) MP.t ; z_crossings : int}
  [@@deriving show]
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
  let empty = {map = MP.empty; z_crossings = 0}
end

module Notary =
struct
  type t = ((pos * side * token),amount) MP.t
  [@@deriving show]
  let empty = MP.empty
  let read n pos side tk = MP.find n (pos,side,tk) |? 0
  let add n pos side tk a =
    let v = read n pos side tk in
    MP.set n (pos,side,tk) (a+v)
end

exception Throws of string
