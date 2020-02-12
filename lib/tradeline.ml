include Tradeline_types
open Tools

let next tl pos = MP.find tl.next pos

let transfer tl pos addr = {tl with owners = MP.set tl.owners pos addr}

let ownerOf tl pos =
  MP.find tl.owners pos

let run_test tl reducer ledger seller_pos buyer_pos test_list =
  let depo =
    match reducer with
      Seller -> Ledger.balance ledger (Epos (tl.id, buyer_pos))
    | Buyer -> Ledger.balance ledger (Epos (tl.id, seller_pos))
  in
  let eval = function
      Higher (t,a) -> depo t >= a
    | Lower (t,a) -> depo t < a
  in
  List.for_all eval test_list


let compute_effects tl reducer seller buyer buyer_pos ledger effects =
  let rec eval ledger effects =
    match effects with
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
            eval (Ledger.transferUpTo ledger (Epos (tl.id, buyer_pos)) (Eaddr seller) t a) ls
       end
  in
  eval ledger effects

let reduce tl ledger seller_pos buyer_pos reducer time clause =

  let buyer = MP.find_exn tl.owners buyer_pos in
  let seller = MP.find_exn tl.owners seller_pos in
  let segment = MP.find_exn tl.segments seller_pos in

  (* Possible error conditions *)
  let incoherent_call () =
    MP.find tl.next seller_pos <> Some buyer_pos in
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
    not (run_test tl reducer ledger seller_pos buyer_pos clause.tests)
  in
  if (incoherent_call() || bad_time() || clause_not_found() || test_fail()) then
    raise (Throws "Clause precondition evaluates to false")
  else
    let ledger' =
      compute_effects tl reducer seller buyer buyer_pos ledger clause.effects
    in
    (* Reduce tradeline *)

    (*1. removing buyer position from tl and making seller position point to next buyer*)
    let next = (*seller_pos ---> next_buyer *)
      let next' = match MP.find tl.next buyer_pos with
          None -> tl.next
        | Some pos' -> MP.set tl.next seller_pos pos' in
      MP.remove next' buyer_pos
    in
    let dead = SP.add tl.dead buyer_pos in
    let segments, buyer_segment = MP.takeout tl.segments buyer_pos in

    (*2. update segments depending on the direction of the reduction*)

    let tl' = {tl with segments ; next ; dead } in
    match reducer with

    (* backward move *)
    | Seller -> (tl',ledger')

    (* forward move *)
    | Buyer ->
       (* reassign buyer_pos's segment to seller_pos (if there is one) *)
       let segments = MP.change segments seller_pos (fun _ -> buyer_segment) in
       (*transferring ownership of seller_pos to buyer*)
       (transfer {tl' with segments} seller_pos buyer, ledger')

let collectable tl pos = tl.source = pos || SP.mem tl.dead pos

let collect tl ledger pos t =
  if collectable tl pos then
    Ledger.transferAll ledger (Epos (tl.id, pos)) (Eaddr (MP.find_exn tl.owners pos)) t
  else
    (* Or just do nothing? *)
    raise (Throws "Position is not collectable")

let init id addr =
  let pos = 0 in
  {
    id = id;
    source = pos;
    max_pos = pos;
    owners = MP.singleton pos addr;
    next = MP.empty;
    segments = MP.empty;
    dead = SP.empty;
  }

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
   segments = MP.set tl.segments pos segment;
  }

