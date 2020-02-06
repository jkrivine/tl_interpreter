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

(*!!! Function does not deal with repeated tests of the same amount !!!!*)
let rec run_tests seller_deposit buyer_deposit = function
    (BuyerHas a)::tests -> run_tests (buyer_deposit-a) seller_deposit tests
  | (SellerHas a)::tests -> run_tests buyer_deposit (seller_deposit-a) tests
  | (Not t')::tests -> not (run_tests buyer_deposit seller_deposit [t']) && (run_tests buyer_deposit seller_deposit tests)
  | [] -> true

(**Generate the (positive) payoff <+s (from contract), +b (from emptying the deposit)> *)
let compute_effects buyer_deposit a =
  if buyer_deposit >= a then (a,buyer_deposit-a)
  else (buyer_deposit,0)


(*Current implementation does not allow one to do zero crossing, test should return a payoff*)
let reduce tl segment_pos reducer time clause =

  let buyer_pos = MP.find_exn tl.next segment_pos in
  let buyer_deposit = MP.find tl.provision buyer_pos |? 0 in

  let seller_deposit = MP.find tl.provision segment_pos |? 0 in
  let segment = MP.find_exn tl.segments segment_pos in


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
    let (s,b) =
      compute_effects buyer_deposit clause.effect
    in
    let payoff = [(ownerOf tl segment_pos,s) ; (ownerOf tl buyer_pos,b)] in
    (* Reduce tradeline *)

    (*1. removing buyer position from tl and making seller position point to next buyer*)
    let next_buyer = MP.find tl.next buyer_pos in (*possibly None*)
    let next = (*segment_pos ---> next_buyer *)
      MP.remove (match next_buyer with None -> tl.next | Some pos' -> MP.set tl.next segment_pos pos') buyer_pos
    in
    let provision = MP.remove tl.provision buyer_pos in

    (*2. update segments depending on the direction of the reduction*)

    match reducer with

    (* backward move *)
    | Seller ->
       let segments = MP.remove tl.segments buyer_pos in
       (transfer {tl with segments ; next ; provision} buyer_pos None,payoff) (*burning buyer_pos*)

    (* forward move *)
    | Buyer ->
       let segments =
         match MP.find tl.segments buyer_pos with
           Some segment' -> MP.set tl.segments segment_pos segment'
         | None -> MP.remove tl.segments segment_pos
       in
       (transfer {tl with segments ; next ; provision} segment_pos (ownerOf tl buyer_pos),payoff) (*transfering ownership of seller_pos to buyer*)

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

