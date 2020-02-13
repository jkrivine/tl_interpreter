module T = Tradeline
open Tools

type call = REDUCE of T.tl_id * T.pos * T.pos * T.side * T.clause
          | GROW of T.tl_id * T.pos * T.segment
          | PROVISION of T.tl_id * T.pos * T.token * T.amount
          | COLLECT of T.tl_id * T.pos * T.amount
          | NEW
          | MAKE_CALL of (T.addr -> T.time -> call)

type t = {
  ledger : T.Ledger.t;
  tls : (T.tl_id, T.t) MP.t;
  max_tl_id : T.tl_id;
}

let new_tl m addr =
  let tls = MP.set m.tls m.max_tl_id (T.init m.max_tl_id addr) in
  let max_tl_id = m.max_tl_id + 1 in
  {m with max_tl_id;tls}


(* call_grow_A time d0 t a d1 d2 means:
 * from [time] to [time+d0], no backward
 * from [time+d0+1] to [time+d1], backward allowed if buyer pos has strictly less than [a] of token [t]
 * from [time+d1+1] onwards, backward allowed; it empties pos of token t
  the _A in the term is for "grow, type a" assuming we'll have more grow archetypes in the future
*)
let call_grow_A tl_id pos fwd_contract d0 t a d1 =
  MAKE_CALL (fun _caller time ->
  let bwd_contract = [
    T.make_clause (Some (time+d0+1))    (Some (time+d0+d1)) [T.Lower (t,a)] [];
    T.make_clause (Some (time+d0+d1+1)) None                []              [T.DrawUpTo (t,a)]
  ] in
  GROW (tl_id,pos, { fwd_contract ; bwd_contract }))

let with_tl m tl_id (k: T.t -> T.Ledger.t -> (T.t * T.Ledger.t)) : t =
  match MP.find m.tls tl_id with
    None -> raise (T.Throws "Tradeline not found")
  | Some tl -> let (tl',ledger) = k tl m.ledger in
     let tls = MP.set m.tls tl_id tl' in
     {m with ledger;tls}

let rec one_step m time = function
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
       | caller, PROVISION (tl_id, pos,t,a) ->
         let ledger = T.Ledger.transfer m.ledger (Eaddr caller) (Epos (tl_id, pos)) t a in
         {m with ledger}
       | _, COLLECT (tl_id, pos, t) ->
         with_tl m tl_id (fun tl ledger -> (tl, T.collect tl ledger pos t))
       | caller, MAKE_CALL f -> one_step m time (caller, (f caller time))


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


