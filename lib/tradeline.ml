include Tradeline_types

let (|?) a default = Option.value a ~default
let throw msg = raise (Throws msg)

let transfer tl pos player_opt =
  match player_opt with
    Some addr -> {tl with owners = MP.set tl.owners pos addr}
  | None -> {tl with owners = MP.remove tl.owners pos} (*burning position*)

let ownerOf tl pos =
  MP.find tl.owners pos

(**[provision tl pos a] provisions [a] in the ledger of position [pos]*)
let provision tl pos amount  =
  let provision = MP.update tl.provision pos (fun a_opt -> (a_opt |? 0) + amount) in
  { tl with provision}

let run_test reducer seller_deposit buyer_deposit test_list =
  let depo =
    match reducer with
      Seller -> buyer_deposit
    | Buyer -> seller_deposit
  in
  let eval = function
      Higher a -> depo >= a
    | Lower a -> depo < a
  in
  List.for_all eval test_list


let compute_effects reducer seller buyer buyer_init_depo ledger effects =
  let rec eval ledger effects buyer_current_depo =
    match effects with
      [] -> Ledger.add ledger buyer buyer_current_depo (*left over is given back to owner of buyer_depo*)
    | (Pay a)::tl -> (*Active payment*)
       begin
         match reducer with
           Buyer -> eval (Ledger.transfer ledger buyer seller a) tl buyer_current_depo
         | Seller -> eval (Ledger.transfer ledger seller buyer a) tl buyer_current_depo
       end
    | (DrawUpTo a)::tl ->
       begin
         match reducer with
           Buyer -> failwith "Illegal passive payment in a forward clause"
         | Seller -> (*passive payment to seller taps into depo*)
            let a' = min buyer_current_depo a in
            eval (Ledger.add ledger seller a') tl (buyer_current_depo-a')
       end
  in
  eval ledger effects buyer_init_depo

(*Current implementation does not allow one to do zero crossing, test should return a payoff*)
let reduce tl ledger seller_pos reducer time clause =

  let buyer_pos = MP.find_exn tl.next seller_pos in
  let buyer = MP.find_exn tl.owners buyer_pos in
  let buyer_deposit = MP.find tl.provision buyer_pos |? 0 in

  let seller = MP.find_exn tl.owners seller_pos in
  let seller_deposit = MP.find tl.provision seller_pos |? 0 in
  let segment = MP.find_exn tl.segments seller_pos in

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
    not (run_test reducer seller_deposit buyer_deposit clause.tests)
  in
  if (bad_time() || clause_not_found() || test_fail()) then
    throw "Clause precondition evaluates to false"
  else
    let ledger' =
      compute_effects reducer seller buyer buyer_deposit ledger clause.effects
    in
    (* Reduce tradeline *)

    (*1. removing buyer position from tl and making seller position point to next buyer*)
    let next_buyer = MP.find tl.next buyer_pos in (*possibly None*)
    let next = (*seller_pos ---> next_buyer *)
      let next' = match next_buyer with
          None -> tl.next
        | Some pos' -> MP.set tl.next seller_pos pos' in
      MP.remove next' buyer_pos
    in
    let provision = MP.remove tl.provision buyer_pos in

    (*2. update segments depending on the direction of the reduction*)

    match reducer with

    (* backward move *)
    | Seller ->
       let segments = MP.remove tl.segments buyer_pos in
       let tl' = {tl with segments ; next ; provision} in
       (transfer tl' buyer_pos None, ledger') (*burning buyer_pos*)

    (* forward move *)
    | Buyer ->
       let segments =
         match MP.find tl.segments buyer_pos with
           Some segment' -> MP.set tl.segments seller_pos segment'
         | None -> MP.remove tl.segments seller_pos
       in
       let tl' = {tl with segments ; next ; provision} in
       (transfer tl' seller_pos (ownerOf tl buyer_pos), ledger') (*transferring ownership of seller_pos to buyer*)

let init addr =
  let pos = 0 in
  {
    source = pos;
    max_pos = pos;
    owners = MP.singleton pos addr;
    next = MP.empty;
    segments = MP.empty;
    provision = MP.empty
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
   segments = MP.set tl.segments pos segment;
  }

