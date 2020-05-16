open Tools
(** Syntactic sugar *)
open Env.Imp.Program
module MP = MP
module SP = SP
module A = Address

type token = A.t
[@@deriving show]

type pos = A.t
[@@deriving show]

type amount = int

(*type t = {map : ((A.t * string * token),amount) MP.t ; zcrossings : int; zwrapping: bool}*)
type t = {map : (A.t,((string * token),amount) MP.t) MP.t ; zcrossings : int; zwrapping: bool}

let solvent hk =
  let {zcrossings;_} = data_get hk in
  return (zcrossings = 0)

let zwrap_start hk =
  data_update hk (fun l -> {l with zwrapping = true})

let zwrap_end hk =
  data_update hk (fun l -> {l with zwrapping = false}) ;
  if solvent hk
  then return ()
  else error "ledger is not solvent"

let is_zwrapping hk =
  let {zwrapping;_} = data_get hk in
  return zwrapping

let find m (a,s,t) =
  return Base.Option.((MP.find m a) >>= fun m -> MP.find m (s,t))

let set m (a,s,t) v =
  MP.update m a (fun m ->
      let m' = m |? MP.empty in
      MP.set m' (s,t) v)


let find_exn hk k =
  match find hk k with
  | Some e -> return e
  | None -> error "not found"
[@@ocaml.warning "-32"]

let balance hk (who:A.t) ?(index="") (tk:token) =
  return (find (data_get hk).map (who,index,tk) |? 0)

let add hk (who:A.t) ?(index="") (tk:token) (a:amount) =
  let v = balance hk who ~index tk in
  data_update hk (fun l ->
      let z = l.zcrossings in
      let modifier = if (a+v<0 && v>=0) then 1 else if (a+v>=0 && v<0) then (-1) else 0 in
      {l with map = set l.map (who,index,tk) (a+v);zcrossings = (z+modifier)}) ;
  let {zwrapping;_} = data_get hk in
  if zwrapping
  then return ()
  else zwrap_end hk

let transfer hk (giver:A.t) ?indices:((i1,i2)=("","")) (tk:token) (a:amount) (taker:A.t) =
  add hk giver ~index:i1 tk (-a) ; add hk taker ~index:i2 tk a

let transfer_up_to hk (giver:A.t) ?indices:((i1,i2)=("","")) (tk:token) (a:amount) (taker:A.t) =
  let b = balance hk giver ~index:i1 tk in
  transfer hk giver ~indices:(i1,i2) tk (min a b) taker

let transfer_all hk (giver:A.t) ?indices:((i1,i2)=("","")) (tk:token) (taker:A.t) =
  let a = balance hk giver ~index:i1 tk in
  transfer hk giver ~indices:(i1,i2) tk a taker

let pp_custom fmt l f =
  let printer fmt addr m' =
    let printer' fmt (index,tk) amount =
      f fmt addr index tk amount in
    MP.pp_i fmt printer' m' in
  MP.pp_i fmt printer l.map


let pp fmt l =
  F.p fmt "(%d zcrossings, zwrapping: %b)" l.zcrossings l.zwrapping;
  pp_custom fmt l (fun fmt addr index tk amount ->
      let index_str = if index = "" then "" else ("."^index) in
      F.cr (); F.p fmt "%a%s has %d %a" A.pp addr index_str amount pp_token tk)

let empty = { map = MP.empty; zcrossings = 0; zwrapping = false}

module Offchain = struct

let iter_address_i hk (who:A.t) ?(index="") f =
  let {map;_} = data_get hk in
  match MP.find map who with
  | None -> ()
  | Some map' ->
    MP.iteri map' (fun (index',token) amount ->
        if index = index' then f token amount else ())

let restrict hk f =
  let l = data_get hk in
  let map = MP.filter l.map @@ fun o m' -> f o m' in
  data_set hk {l with map}

let update hk f =
  let l = data_get hk in
  let map = MP.map l.map @@ fun owner m' ->
      MP.map m' @@ fun (index,token) amount -> f (owner,index,token) amount
  in data_set hk {l with map}
end
