open Tools
open Env
(** Syntactic sugar *)
module MP = MP
module SP = SP
module A = Address

type token = A.t
[@@deriving show]

type pos = A.t
[@@deriving show]

type amount = int

type t = {map : ((A.t * string * token),amount) MP.t ; zcrossings : int; zwrapping: bool}

let solvent hk =
  let* {zcrossings;_} = data_get hk in
  return (zcrossings = 0)

let zwrap_start hk =
  data_update hk (fun l -> {l with zwrapping = true})

let zwrap_end hk =
  data_update hk (fun l -> {l with zwrapping = false}) >>
  let* is_solvent = solvent hk in
  if is_solvent
  then return ()
  else error "ledger is not solvent"

let is_zwrapping hk =
  let* {zwrapping;_} = data_get hk in
  return zwrapping

let find hk k =
  let* l = data_get hk in
  return (MP.find l.map k)

let find_exn hk k =
  find hk k >>= function
  | Some e -> return e
  | None -> error "not found"
[@@ocaml.warning "-32"]

let balance hk (who:A.t) ?(index="") (tk:token) =
  let* l = data_get hk in
  return (MP.find l.map (who,index,tk) |? 0)

let add hk (who:A.t) ?(index="") (tk:token) (a:amount) =
  let* v = balance hk who ~index tk in
  data_update hk (fun l ->
      let z = l.zcrossings in
      let modifier = if (a+v<0 && v>=0) then 1 else if (a+v>=0 && v<0) then (-1) else 0 in
      {l with map = MP.set l.map (who,index,tk) (a+v);zcrossings = (z+modifier)}) >>
  let* {zwrapping;_} = data_get hk in
  let* is_solvent = solvent hk in
  if zwrapping || is_solvent
  then return ()
  else error "ledger is not solvent"

let transfer hk (giver:A.t) ?indices:((i1,i2)=("","")) (tk:token) (a:amount) (taker:A.t) =
  add hk giver ~index:i1 tk (-a) >> add hk taker ~index:i2 tk a

let transfer_up_to hk (giver:A.t) ?indices:((i1,i2)=("","")) (tk:token) (a:amount) (taker:A.t) =
  let* b = balance hk giver ~index:i1 tk in
  transfer hk giver ~indices:(i1,i2) tk (min a b) taker

let transfer_all hk (giver:A.t) ?indices:((i1,i2)=("","")) (tk:token) (taker:A.t) =
  let* a = balance hk giver ~index:i1 tk in
  transfer hk giver ~indices:(i1,i2) tk a taker

let pp fmt l =
  F.p fmt "(%d zcrossings, zwrapping: %b)" l.zcrossings l.zwrapping;
  let printer fmt (addr,index,t) amount = F.cr (); F.p fmt "%a.%s has %d%a" A.pp addr index amount pp_token t in
  MP.pp_i fmt printer l.map

let empty = { map = MP.empty; zcrossings = 0; zwrapping = false}
