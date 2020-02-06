module T = Tradeline

type tl_id = int

type reduction_command = {
  seller_pos: T.pos;
  reducer: T.side;
  clause: T.clause
}

type t = {
  ledger : T.Ledger.t;
  tls : (tl_id, T.t) MP.t;
  max_tl_id : tl_id;
}

let reduce m tl_id time lst = match MP.find m.tls tl_id with
    None -> failwith "Tradeline not found"
  | Some tl ->
    let rec reduce' tl ledger = function
      | [] -> if T.Ledger.solvent ledger
        then
          let tls = MP.set m.tls tl_id tl in
          {m with ledger;tls}
        else
          failwith "Resulting mother contract is not solvent"
      | {seller_pos ; reducer; clause}::ls ->
        let tl', ledger' = T.reduce tl m.ledger seller_pos reducer time clause in
        reduce' tl' ledger' ls
    in reduce' tl m.ledger lst

let new_tl m addr =
  let max_tl_id = m.max_tl_id + 1 in
  let tls = MP.set m.tls max_tl_id (T.init addr) in
  {m with max_tl_id;tls}


