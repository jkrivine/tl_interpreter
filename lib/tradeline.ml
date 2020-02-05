include Tradeline_types

let (|?) a default = Option.value a ~default
let throw msg = raise (Throws msg)

let transfer tl pos player =
  {tl with owners = MP.set tl.owners pos player}

let default_segment = {
  fwd_contract = MP.empty;
  bwd_contract = [];
}

(**[provision tl pos addr a] [addr] provisions [a] in the ledger of the segement at position [pos]*)
let provision tl pos amount  =
  let provision = MP.update tl.provision pos (fun a_opt -> (a_opt |? 0) + amount) in
  { tl with provision}

(*!!! Function does not deal with repeated tests of the same amount !!!!*)
let rec run_tests buyer_deposit seller_deposit = function
    (BuyerHas a)::tests -> (buyer_deposit >= a) && (run_tests buyer_deposit seller_deposit tests)
  | (SellerHas a)::tests -> (buyer_deposit >= a) && (run_tests buyer_deposit seller_deposit tests)
  | (Not t')::tests -> not (run_tests buyer_deposit seller_deposit [t']) && (run_tests buyer_deposit seller_deposit tests)
  | [] -> true

(**Generate the required payoff w/o applying it to the ledger*)
let compute_effects buyer_deposit a =
  if buyer_deposit >= a then (a,buyer_deposit-a)
  else (buyer_deposit,0)

let reduce tl segment_pos reducer time clause =

  let buyer_pos = MP.find_exn tl.next segment_pos in
  let seller_pos = segment_pos in
  let segment = MP.find_exn tl.segments segment_pos in
  let seller_deposit = MP.find tl.provision segment_pos |? 0 in
  let buyer_deposit = MP.find tl.provision buyer_pos |? 0 in

  (* Possible error conditions *)
  let bad_time () =
    (clause.t_from <> None && Option.get clause.t_from > time)
    || (clause.t_to <> None && Option.get clause.t_to < time)
  in
  let clause_not_found () = (*Eventually will be a test that a given clause hsh indeed matches a clause*)
    match reducer with
    | Seller -> not (List.mem clause segment.bwd_contract)
    | Buyer -> (match MP.find segment.fwd_contract segment_pos with
        | None -> true (*No fwd contract is attached to seller_pos, this should be impossible ?*)
        | Some lst -> not (List.mem clause lst))
  in
  let test_fail () =
    not (run_tests seller_deposit buyer_deposit clause.tests)
  in
  if (bad_time() || clause_not_found() || test_fail()) then
    throw "Clause precondition evaluates to false"
  else
    let seller_payoff,buyer_payoff =
      compute_effects buyer_deposit clause.effect
    in

    (* Reduce tradeline *)
    let next' = MP.remove (MP.remove tl.next buyer_pos) seller_pos in
    let next = match MP.find tl.next buyer_pos with
      | None -> next'
      | Some pos' -> MP.set next' seller_pos pos'
    in
    match reducer with
    (* backward move *)
    | Seller ->
      let segments = (match MP.find tl.segments buyer_pos with
          | None -> MP.remove (MP.set tl.segments buyer_pos segment) seller_pos
          | Some _ -> MP.set tl.segments seller_pos segment) in
      { tl with segments; next}
    (* forward move *)
    | Buyer ->
       let owners = MP.set tl.owners seller_pos buyer in
       let segments' = (match MP.find tl.segments buyer_pos with
                        | None -> MP.remove tl.segments seller_pos
                        | Some segment' -> MP.set tl.segments seller_pos segment') in
       let segments = MP.set segments' buyer_pos segment in
       { tl with owners; segments; next}

(* Orphan segments are globally gc-able
   segments where [addr] is not a party in the ledger is addr gc-able.
*)
let gc tl segment_pos addr =
  match MP.find tl.segments segment_pos with
  | None -> (tl,0)
  | Some segment ->
    match MP.find segment.ledger addr with
    | None -> (tl,0)
    | Some amount ->
      let can_gc = (match MP.find tl.next segment_pos with
          (* No next pos with associated segment means an orphaned token+segment *)
          | None -> true
          (* Otherwise, gc possible if addr is not a current party to the segment *)
          | Some buyer_pos -> addr <> MP.find_exn tl.owners segment_pos && addr <> MP.find_exn tl.owners buyer_pos) in
      if can_gc then
        let ledger = MP.set segment.ledger addr 0 in
        let segments = MP.set tl.segments segment_pos {segment with ledger} in
        ({ tl with segments }, amount)
      else (tl,0)


let init addr =
  let pos = 0 in
  {
    source = pos;
    max_pos = pos;
    owners = MP.singleton pos addr;
    next = MP.empty;
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
   underlying = None;
   segments = MP.set tl.segments pos segment;
  }

let bind tl asset =
  let underlying = match tl.underlying with
       | None -> Some asset
       | Some _ -> failwith "Cannot change underlying of a tradeline" in
  { tl with underlying }


let unbind tl = ({ tl with underlying = None }, tl.underlying)

