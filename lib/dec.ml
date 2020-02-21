include Dec_types
open Tools

module MP = MP


type call = REDUCE of pos * pos * side * clause
          | GROW of pos * segment
          | PROVISION of pos * token * amount
          | COLLECT of pos * token
          | NEW
          | MAKE_CALL of (addr -> time -> call)
[@@deriving show]

type t = {
  (** Store of funds owned by addresses and positions *)
  ledger : Ledger.t;
  (* Registry of position ownership *)
  owners: (pos, addr) MP.t;
  (* Source set. 'source' must be testable for collection pruposes. *)
  sources : pos SP.t;
  (* Tradeline structure: u -> u+ position map *)
  next: (pos,pos) MP.t;
  (* [segments.find u] returns the segment between u and u+*)
  (* !!Warning: segments is invariant under backward but is modified by forward.*)
  segments : (pos, segment) MP.t;
  (* Dead set. Non-sources are collectable when they are dead, and dead positions cannot be grown *)
  dead: pos SP.t;
  (* todo: oracles *)
  oracles: addr -> Oracle.t;
  (* Source of fresh pos numbers; could be random int *)
  max_pos: pos;
}
[@@deriving show]

let empty =
  {
    ledger= Ledger.empty;
    owners= MP.empty;
    sources= SP.empty;
    next= MP.empty;
    segments= MP.empty;
    dead= SP.empty;
    oracles = (fun _-> failwith "woops");
    max_pos= 0;
  }


(** *)

let new_pos m = ({m with max_pos=m.max_pos + 1}, m.max_pos)

let init_tl m addr =
  let m',source = new_pos m in
  let sources = SP.add m.sources source in
  let owners = MP.set m.owners source addr in
  ({ m' with sources ; owners }, source)

let next m pos = MP.find m.next pos

let transfer_pos m pos addr = {m with owners = MP.set m.owners pos addr}

let ownerOf m pos = MP.find m.owners pos

let run_test m reducer seller_pos buyer_pos test_list =
  let depo = match reducer with
      Seller -> Ledger.balance m.ledger (Epos buyer_pos)
    | Buyer -> Ledger.balance m.ledger (Epos seller_pos)
  in
  let eval = function
      ProvisionHte (t,a) -> depo t >= a
    | ProvisionLt (t,a) -> depo t < a
    (*| OracleHte (addr,a) -> Oracles.read addr >= a*)
    (*| ProvisionLt (addr,a) -> Oracles.read addr < a*)
  in
  List.for_all eval test_list


let compute_effects m reducer seller buyer buyer_pos effects =
  let rec eval ledger effects = match effects with
    (* with multiple tokens, we do not give back the remainder to buyer anylonger *)
      [] -> ledger
    | (Pay (t,a))::ls -> (*Active payment*)
       begin
         match reducer with
           Buyer -> eval (Ledger.transfer ledger (Eaddr buyer) (Eaddr seller) t a) ls
         | Seller -> eval (Ledger.transfer ledger (Eaddr seller) (Eaddr buyer) t a) ls
       end
    | (DrawUpTo (t,a))::ls ->
       begin
         match reducer with
           Buyer -> failwith "Illegal passive payment in a forward clause"
         | Seller -> (*passive payment to seller taps into depo*)
            eval (Ledger.transferUpTo ledger (Epos buyer_pos) (Eaddr seller) t a) ls
       end
  in
  {m with ledger = eval m.ledger effects}

let reduce m seller_pos buyer_pos reducer time clause =

  let buyer = MP.find_exn m.owners buyer_pos in
  let seller = MP.find_exn m.owners seller_pos in
  let segment = MP.find_exn m.segments seller_pos in

  (* Possible error conditions *)
  let incoherent_call () =
    MP.find m.next seller_pos <> Some buyer_pos in
  let bad_time () =
    (clause.t_from <> None && Option.get clause.t_from > time)
    || (clause.t_to <> None && Option.get clause.t_to < time)
  in
  let clause_not_found () = (*Eventually will be a test that a given clause hsh indeed matches a clause*)
    match reducer with
    | Seller -> not (List.mem clause segment.bwd_contract)
    | Buyer -> (match MP.find segment.fwd_contract seller_pos with
        | None -> true (*No fwd contract is attached to seller_pos, this should be impossible ?*)
        | Some lst -> not (List.mem clause lst))
  in
  let test_fail () =
    not (run_test m reducer seller_pos buyer_pos clause.tests)
  in
  if (incoherent_call() || bad_time() || clause_not_found() || test_fail()) then
    raise (Throws "Clause precondition evaluates to false")
  else
    let m' = compute_effects m reducer seller buyer buyer_pos clause.effects
    in
    (* Reduce tradeline *)

    (*1. removing buyer position from tl and making seller position point to next buyer*)
    let next = (*seller_pos ---> next_buyer *)
      let next' = match MP.find m.next buyer_pos with
          None -> m.next
        | Some pos' -> MP.set m.next seller_pos pos' in
      MP.remove next' buyer_pos
    in
    let dead = SP.add m.dead buyer_pos in
    let segments, buyer_segment = MP.takeout m.segments buyer_pos in

    (*2. update segments depending on the direction of the reduction*)

    let m'' = {m' with segments ; next ; dead } in
    match reducer with

    (* backward move *)
    | Seller -> m''

    (* forward move *)
    | Buyer ->
       (* reassign buyer_pos's segment to seller_pos (if there is one) *)
       let segments = MP.change segments seller_pos (fun _ -> buyer_segment) in
       (*transferring ownership of seller_pos to buyer*)
       transfer_pos {m'' with segments} seller_pos buyer

(**Collect dead position or singleton source *)
let collectable m pos = (SP.mem m.sources pos && MP.find m.next pos = None) || SP.mem m.dead pos

let collect m pos t =
  if collectable m pos then
    let ledger = Ledger.transferAll m.ledger (Epos pos) (Eaddr (MP.find_exn m.owners pos)) t in
    let dead = SP.add m.dead pos  in
    { m with ledger ; dead }
  else
    (* Or just do nothing? *)
    raise (Throws "Position is not collectable")

let rec get_sink m pos = match MP.find m.next pos with
    | None -> pos
    | Some pos' -> get_sink m pos'

let grow m sink segment =
  if SP.mem m.dead sink || MP.mem m.next sink then
    raise (Throws "Position cannot be grown (dead or already with an an attached buyer pos)")
  else
    let m', fresh_pos = new_pos m in
    let owners = MP.set m'.owners fresh_pos (MP.find_exn m'.owners sink)
    and next = MP.set m'.next fresh_pos sink
    and segments = MP.set m'.segments fresh_pos segment in
    { m' with owners ; next ; segments }

let make_clause t_from t_to tests effects = { t_from; t_to; tests; effects }


(* call_grow_A time d0 t a d1 d2 means:
 * from [time] to [time+d0], no backward
 * from [time+d0+1] to [time+d1], backward allowed if buyer pos has strictly less than [a] of token [t]
 * from [time+d1+1] onwards, backward allowed; it empties pos of token t
  the _A in the term is for "grow, type a" assuming we'll have more grow archetypes in the future
*)
let call_grow_A pos fwd_contract d0 t a d1 =
  MAKE_CALL (fun _caller time ->
  let bwd_contract = [
    make_clause (Some (time+d0+1))    (Some (time+d0+d1)) [ProvisionLt (t,a)] [];
    make_clause (Some (time+d0+d1+1)) None                []              [DrawUpTo (t,a)]
  ] in
  GROW (pos, { fwd_contract ; bwd_contract }))

let rec one_step m time = function
  | caller, NEW ->
    let m',_ = init_tl m caller in m'
  | caller, REDUCE (seller_pos,buyer_pos,reducer,clause) ->
    let subject_pos = if reducer = Seller then seller_pos else buyer_pos in
    if Some caller <> (ownerOf m subject_pos) then
      raise (Throws "Caller not authorized to reduce")
    else
      reduce m seller_pos buyer_pos reducer time clause
  | caller, GROW (seller_pos,segment) ->
    if not (Some caller = (ownerOf m seller_pos)) then
      raise (Throws "Caller not authorized to reduce")
    else
      grow m seller_pos segment
  | caller, PROVISION (pos,tk,a) ->
    let ledger = Ledger.transfer m.ledger (Eaddr caller) (Epos pos) tk a in
    {m with ledger}
  | _, COLLECT (pos, tk) ->
    collect m pos tk
  | caller, MAKE_CALL f ->
    one_step m time (caller, (f caller time))


let exec m time calls =
     let m =
       List.fold_left
         (fun m' call -> one_step m' time call
         ) m calls
     in
     if Ledger.solvent m.ledger
     then m
     else
       raise (Throws "Reduction sequence is not solvent")
