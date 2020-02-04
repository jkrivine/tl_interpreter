include Tradeline_types

let (|?) a default = Option.value a ~default
let throw msg = raise (Throws msg)

let ledger_balance ledger player =
  Option.value (MP.find ledger player) ~default:0

let transfer tl pos player =
  {tl with owners = MP.set tl.owners pos player}

let default_segment = {
  fwd_contract = MP.empty;
  bwd_contract = [];
  ledger = MP.empty
}

(**[provision tl pos addr a] [addr] provisions [a] in the ledger of the segement at position [pos]*)
let provision tl pos addr amount  =
  let segments = MP.update tl.segments pos (function
      | None -> throw "Segment does not exist"
      | Some segment ->
        let ledger = MP.update segment.ledger addr (fun a_opt -> (a_opt |? 0) + amount) in
        { segment with ledger})
  in { tl with segments }

let run_tests seller_deposit buyer_deposit asset_opt tests =
  let rec run_test' = function
    | Not t -> not (run_test' t)
    | LedgerHas (amount,side) -> (match side with
                                  | Seller -> seller_deposit >= amount
                                  | Buyer -> buyer_deposit >= amount)
    | TradeLineHas asset' -> (match asset_opt with
                              | Some asset -> asset = asset'
                              | None -> false) in
  List.for_all run_test' tests


(**Generate the required payoff w/o applying it to the ledger*)
let compute_effects effects : amount =
  let rec compute_effects' seller_payoff = function
    | [] -> seller_payoff
    | Give (a,Seller)::ls -> compute_effects' (seller_payoff + a) ls
    | Give (a,Buyer)::ls -> compute_effects' (seller_payoff - a) ls in
  compute_effects' 0 effects

let reduce tl seller_pos reducer time clause =
  let buyer_pos = MP.find_exn tl.next seller_pos in
  let segment = MP.find_exn tl.segments seller_pos in
  let seller = MP.find_exn tl.owners seller_pos in
  let buyer = MP.find_exn tl.owners buyer_pos in
  let seller_deposit = MP.find segment.ledger seller |? 0 in
  let buyer_deposit = MP.find segment.ledger buyer |? 0 in

  (* Possible error conditions *)
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
    not (run_tests seller_deposit buyer_deposit tl.underlying clause.tests)
  in
  if (bad_time() || clause_not_found() || test_fail()) then
    throw "Clause precondition evaluates to false"
  else
    (* Apply payoffs to segment ledger *)
    let seller_payoff = compute_effects clause.effects in (* buyer payoff is symmetric, seller_payoff may be negative *)
    let seller_final = seller_deposit + seller_payoff in
    let seller_bal,buyer_bal =
      if seller_final < 0
      then 0, buyer_deposit + seller_deposit (*Seller defaults*)
      else
        let buyer_final = buyer_deposit - seller_payoff in
        if buyer_final < 0
        then seller_deposit + buyer_deposit, 0 (*Buyer defaults*)
        else seller_final,buyer_final in
    let segment = {segment with ledger = MP.set (MP.set segment.ledger buyer buyer_bal) seller seller_bal} in

    (* Reduce tradeline *)
    let next' = MP.remove (MP.remove tl.next buyer_pos) seller_pos
    and prev' = MP.remove tl.prev buyer_pos in
    let next,prev = match MP.find tl.next buyer_pos with
      | None -> next', prev'
      | Some pos' -> MP.set next' seller_pos pos', MP.set prev' pos' seller_pos in
    match reducer with
    (* backward move *)
    | Seller ->
      let segments = (match MP.find tl.segments buyer_pos with
          | None -> MP.remove (MP.set tl.segments buyer_pos segment) seller_pos
          | Some _ -> MP.set tl.segments seller_pos segment) in
      { tl with segments; next; prev }
    (* forward move *)
    | Buyer ->
       let owners = MP.set tl.owners seller_pos buyer in
       let segments' = (match MP.find tl.segments buyer_pos with
                        | None -> MP.remove tl.segments seller_pos
                        | Some segment' -> MP.set tl.segments seller_pos segment') in
       let segments = MP.set segments' buyer_pos segment in
       { tl with owners; segments; next; prev}

(* Implements gc for dead segments & segments where [addr] is not a party.
   Does not implement time-based gc
*)
let gc tl seller_pos addr =
  match MP.find tl.segments seller_pos with
  | None -> (tl,0)
  | Some segment ->
    match MP.find segment.ledger addr with
    | None -> (tl,0)
    | Some amount ->
      let can_gc = (match MP.find tl.next seller_pos with
          (* No next pos with associated segment means an orphaned token+segment *)
          | None -> true
          (* Otherwise, gc possible if addr is not a current party to the segment *)
          | Some buyer_pos -> addr <> MP.find_exn tl.owners seller_pos && addr <> MP.find_exn tl.owners buyer_pos) in
      if can_gc then
        let ledger = MP.set segment.ledger addr 0 in
        let segments = MP.set tl.segments seller_pos {segment with ledger} in
        ({ tl with segments }, amount)
      else (tl,0)


let init addr =
  let pos = 0 in
  {
    source = pos;
    max_pos = pos;
    owners = MP.singleton pos addr;
    next = MP.empty;
    prev = MP.empty;
    underlying = None;
    segments = MP.empty;
  }

(* should be cached in concrete implementations *)
let get_sink tl =
  let rec get_sink' pos = match MP.find tl.next pos with
    | None -> pos
    | Some pos' -> get_sink' pos' in
  get_sink' tl.source


let grow tl segment =
  let pos = tl.max_pos + 1 in
  let sink = get_sink tl in
  {tl with
   max_pos = pos;
   owners = MP.set tl.owners pos (MP.find_exn tl.owners sink);
   next = MP.set tl.next pos sink;
   prev = MP.set tl.prev sink pos;
   underlying = None;
   segments = MP.set tl.segments pos segment;
  }


let bind tl asset =
  let underlying = match tl.underlying with
       | None -> Some asset
       | Some _ -> failwith "Cannot change underlying of a tradeline" in
  { tl with underlying }

let unbind tl = ({ tl with underlying = None }, tl.underlying)

