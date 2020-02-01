include Tradeline_types


let (|?) a default = Option.value a ~default

let ledger_balance ledger player =
  Option.value (MP.find ledger player) 0

let transfer tl pos player =
  {tl with owners = MP.set tl.owners pos player}

let default_segment = {
  fwd_contract = MP.empty;
  bwd_contract = [];
  ledger = MP.empty
}

let provision tl pos player amount =
  let segments = MP.update tl.segments pos
  (fun s_opt ->
    let segment = s_opt |? default_segment in
    let update_amount a_opt = (amount + (a_opt |? 0)) in
    let ledger = MP.update segment.ledger player update_amount in
    {segment with ledger}
  )
    in {tl with segments}


(*val reduce : tradeline -> pos -> side -> time -> clause -> (tradeline * payoff)*)

let run_tests seller_a buyer_a asset_opt tests =
  let rec run_test' = function
    | Not t -> not (run_test' t)
    | LedgerHas (amount,side) -> (match side with
      | Seller -> seller_a >= amount
      | Buyer -> buyer_a >= amount)
    | TradeLineHas asset' -> (match asset_opt with
      | Some asset -> asset = asset'
      | None -> false) in
  List.for_all run_test' tests


let run_effects effects : amount=
  let rec run_effects' seller_payoff = function
    | [] -> seller_payoff
  | Give (a,Seller)::ls -> run_effects' (seller_payoff + a) ls
  | Give (a,Buyer)::ls -> run_effects' (seller_payoff - a) ls in
  run_effects' 0 effects

let reduce tl seller_pos reducer time clause =
  let buyer_pos = MP.find_exn tl.next seller_pos in
  let segment = MP.find_exn tl.segments seller_pos in
  let seller = MP.find_exn tl.owners seller_pos in
  let buyer = MP.find_exn tl.owners buyer_pos in
  let seller_amount = MP.find segment.ledger seller |? 0 in
  let buyer_amount = MP.find segment.ledger buyer |? 0 in

  (* Possible error conditions *)
  let bad_time =
    (clause.t_from <> None && Option.get clause.t_from > time)
    || (clause.t_to <> None && Option.get clause.t_to < time) in
  let clause_not_found =
    match reducer with
      | Seller -> not (List.mem clause segment.bwd_contract)
      | Buyer -> (match MP.find segment.fwd_contract seller_pos with
        | None -> false
        | Some lst -> List.mem clause lst) in
  let test_fail =
    not (run_tests seller_amount buyer_amount tl.underlying clause.tests) in

  if bad_time || clause_not_found || test_fail then
    raise Throws
  else
    (* Apply payoffs to segment ledger *)
    let seller_payoff = run_effects clause.effects in
    let seller_final = seller_amount + seller_payoff in
    let s,b =
    if seller_final < 0 then 0, buyer_amount + seller_amount
    else
      let buyer_final = buyer_amount - seller_payoff in
      if buyer_final < 0 then seller_amount + buyer_amount, 0
      else seller_final,buyer_final
    in
    let segment = {segment with ledger=MP.set (MP.set segment.ledger buyer b)
    seller s} in

  (* Reduce tradeline *)
  let next',prev' = match MP.find tl.next buyer_pos with
  | None -> tl.next,tl.prev
  | Some pos' -> MP.set tl.next seller_pos buyer_pos, MP.set tl.prev
  buyer_pos seller_pos in

  let next, prev = MP.remove next' buyer_pos, MP.remove prev' buyer_pos in
  match reducer with
  | Seller -> { tl with next;prev }
  | Buyer ->
      let owners = MP.set tl.owners seller_pos buyer in
      let segments' = match MP.find tl.segments buyer_pos with
      | None -> tl.segments
      | Some s -> MP.set tl.segments seller_pos s in
      let segments = MP.set segments' buyer_pos segment in
      {tl with owners;segments;next;prev}

let gc tl pos side time p_opt clause = failwith "Not implemented"

let init addr = failwith "Not implemented"

let grow tl segment = failwith "Not implemented"

let bind tl asset = failwith "Not implemented"

let unbind tl = failwith "Not implemented"