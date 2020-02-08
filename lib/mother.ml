module T = Tradeline
include Tools


type tl_id = int

type call = REDUCE of T.pos * T.side * T.clause
          | GROW of T.pos * T.segment
          | PAY of T.pos * T.amount

type t = {
  ledger : T.Ledger.t;
  tls : (tl_id, T.t) MP.t;
  max_tl_id : tl_id;
}

let one_step tl ledger time caller = function
       | REDUCE (seller_pos,reducer,clause) ->
          let subject_pos = match reducer with
              T.Seller -> seller_pos
            | T.Buyer ->
               match T.next tl seller_pos with
                 None -> raise (T.Throws "Illegal buyer position")
               | Some p -> p
          in
          if not (caller = (T.ownerOf tl subject_pos |? -1)) then
            raise (T.Throws "Caller not authorized to reduce")
          else
            T.reduce tl ledger seller_pos reducer time clause
       | GROW (seller_pos,segment) ->
          begin
            (*check here that seller_pos is head of tl*)
            match T.next tl seller_pos with
              Some _ -> raise (T.Throws "Cannot grow a positionn that is not head of a tradeline")
            | None ->
               (T.grow tl segment,ledger)
          end
       | PAY (pos,a) ->
          (*update ledger here*)
          (T.provision tl pos a,ledger)

let exec m caller tl_id time calls =
  match MP.find m.tls tl_id with
    None -> raise (T.Throws "Tradeline not found")
  | Some tl ->
     let tl,ledger =
       List.fold_left
         (fun (tl,ledger) call -> one_step tl ledger time caller call
         ) (tl,m.ledger) calls
     in
     if T.Ledger.solvent ledger
     then
       let tls = MP.set m.tls tl_id tl in
       {m with ledger;tls}
     else
       raise (T.Throws "Reduction sequence is not solvent")

let new_tl m addr =
  let max_tl_id = m.max_tl_id + 1 in
  let tls = MP.set m.tls max_tl_id (T.init addr) in
  {m with max_tl_id;tls}


