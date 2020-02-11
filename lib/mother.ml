module T = Tradeline
open Tools

type tl_id = int

type call = REDUCE of tl_id * T.pos * T.pos * T.side * T.clause
          | GROW of tl_id * T.pos * T.segment
          | PROVISION of tl_id * T.pos * T.amount
          | COLLECT of T.pos * T.amount
          | NEW

type t = {
  ledger : T.Ledger.t;
  tls : (tl_id, T.t) MP.t;
  max_tl_id : tl_id;
}

let new_tl m addr =
  let max_tl_id = m.max_tl_id + 1 in
  let tls = MP.set m.tls max_tl_id (T.init addr) in
  {m with max_tl_id;tls}

let with_tl m tl_id (k: T.t -> T.Ledger.t -> (T.t * T.Ledger.t)) : t =
  match MP.find m.tls tl_id with
    None -> raise (T.Throws "Tradeline not found")
  | Some tl -> let (tl',ledger) = k tl m.ledger in
     let tls = MP.set m.tls tl_id tl' in
     {m with ledger;tls}

let one_step m time = function
       | caller, NEW -> new_tl m caller
       | caller, REDUCE (tl_id,seller_pos,buyer_pos,reducer,clause) ->
         with_tl m tl_id (fun tl ledger ->
             let subject_pos = if reducer = T.Seller then buyer_pos else seller_pos in
             if Some caller <> (T.ownerOf tl subject_pos) then
               raise (T.Throws "Caller not authorized to reduce")
             else
               T.reduce tl ledger seller_pos buyer_pos reducer time clause
           )
       | caller, GROW (tl_id,seller_pos,segment) ->
         with_tl m tl_id (fun tl ledger ->
             (*check here that seller_pos is head of tl*)
             match T.next tl seller_pos with
               Some _ -> raise (T.Throws "Cannot grow a position that is not head of a tradeline")
             | None ->
               if not (caller = (T.ownerOf tl seller_pos |? -1)) then
                 raise (T.Throws "Caller not authorized to reduce")
               else
                 (T.grow tl segment,ledger)
           )
       | caller, PROVISION (pos,t,a) ->
         let ledger = T.Ledger.transfer m.ledger (Eaddr caller) (Epos pos) t a in
         {m with ledger}
       | caller, COLLECT (pos, a) ->
          (*should provision ledger of ownerOf pos with amount a, provided pos is singleton*)
          failwith "Not implemented yet"

let exec m time calls =
     let m =
       List.fold_left
         (fun m' call -> one_step m' time call
         ) m calls
     in
     if T.Ledger.solvent m.ledger
     then m
     else
       raise (T.Throws "Reduction sequence is not solvent")
