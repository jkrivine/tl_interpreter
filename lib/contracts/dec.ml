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
(*[@@deriving show]*)

(** Generic representation for choosing which side of a tradeline segment we're talking about. *)
type side = Left | Right
[@@deriving show]

module Ledger =
struct
  type t = {map : ((A.t * token),amount) MP.t ; z_crossings : int}

  let find hk k =
    let* l = data_get hk in
    return (MP.find l.map k)

  let find_exn hk k =
    find hk k >>= function
    | Some e -> return e
    | None -> error "not found"
  [@@ocaml.warning "-32"]

  let balance hk (who:A.t) (tk:token) =
    let* l = data_get hk in
    return (MP.find l.map (who,tk) |? 0)

  let add hk (who:A.t) (tk:token) (a:amount) =
    let* v = balance hk who tk in
    data_update hk (fun l ->
        let z = l.z_crossings in
        let modifier = if (a+v<0 && v>=0) then 1 else if (a+v>=0 && v<0) then (-1) else 0 in
        {map = MP.set l.map (who,tk) (a+v);z_crossings = (z+modifier)})

  let transfer hk (giver:A.t) (tk:token) (a:amount) (taker:A.t) =
    add hk giver tk (-a) >> add hk taker tk a

  let transfer_up_to hk (giver:A.t) (tk:token) (a:amount) (taker:A.t) =
    let* b = balance hk giver tk in
    transfer hk giver tk (min a b) taker

  let transfer_all hk (giver:A.t) (tk:token) (taker:A.t) =
    let* a = balance hk giver tk in
    transfer hk giver tk a taker

  let solvent hk =
    let* {z_crossings;_} = data_get hk in
    return (z_crossings = 0)

  let pp fmt l =
    F.p fmt "(%d zcrossings)" l.z_crossings;
    let printer fmt (addr,t) amount = F.cr (); F.p fmt "%a has %d%a" A.pp addr amount pp_token t in
    MP.pp_i fmt printer l.map

  let empty = { map = MP.empty; z_crossings = 0; }

end

(*let pos_names : (pos,string) MP.t data_hkey*)
  (*= data ~init:MP.empty                                    "position names"*)
let ledger    = data ~pp:Ledger.pp                 ~init:Ledger.empty  "ledger"
let owners    = data ~pp:(MP.pp A.pp A.pp) ~init:MP.empty      "owners"
let sources   = data ~pp:(SP.pp A.pp)            ~init:SP.empty      "sources"
let nexts     = data ~pp:(MP.pp A.pp A.pp)     ~init:MP.empty      "nexts"
let segments  = data ~pp:(MP.pp A.pp A.pp) ~init:MP.empty      "segments"
let deads     = data ~pp:(SP.pp A.pp)            ~init:SP.empty      "deads"
(* Positions have two 'boxes', a forward one and a backward one.
   The backward one is tied to the position itself. It cannot be detached.
   The forward one is called its 'box'.
   A box can be owned by anyone: a position, a user, another box, a contract...
   But boxes b come into existence owned by nobody, and attached as the box of a position p. Morally, b is the 'forward box' of p. This does *not* mean that p owns b. In that case, b would morally be in the 'backward box' of p. *)
let boxes     = data ~pp:(MP.pp A.pp A.pp) ~init:MP.empty      "boxes"

let echo_dec =
  echo_data ledger >>
  echo_data owners >>
  echo_data sources >>
  echo_data nexts >>
  echo_data segments >>
  echo_data deads >>
  echo_data boxes

type parties = pos*pos

(* Change tl topology *)
let init_tl : (string * string * A.t, A.t * A.t) code_hkey = code ()
let grow : (parties * string * A.t * A.t, pos) code_hkey = code ()
let pull : (parties, unit) code_hkey = code ()
let commit  : (parties, unit) code_hkey = code ()
(* Transfers *)
let collect_token : (A.t * token, unit) code_hkey = code ()
let collect_address : (A.t * A.t, unit) code_hkey = code ()
let collect_box : (A.t, unit) code_hkey = code ()
let transfer_token : (token * amount * A.t, unit) code_hkey = code ()
let transfer_address : (A.t * A.t, unit) code_hkey = code ()
(* UNSAFE *)
let pay : (parties * side * token * amount * A.t,unit) code_hkey = code ()
(* read info *)
(* owners of boxes&positions are anything *)
let owner_of  : (A.t, A.t) code_hkey = code ()
(* a position may or may not have a box *)
(* pos -> prov *)
let box_of : (A.t, A.t option) code_hkey = code ()
(* any -> ... *)
let balance_of : (A.t * token, amount) code_hkey = code ()
(* Convenience composition of right_prov and get_balance *)
let box_balance_of : (A.t * token, amount) code_hkey = code ()


(* Addresses can be contracts, users, or "just numbers". In particular, positions and provisions are represented by addresses but there is no content related to them in global blockchain storage *)
(* Owner of a position is any address *)


 (*pos -> not pos *)
(*let pos_owner : (A.t,A.t) code_hkey = code ()*)
 (*provision -> any *)
 (*this is only needed because right provisions *)
   (*can end up without an owning position *)
(*let provision_owner : (A.t,A.t) code_hkey = code ()*)

(* a pos can own many ledgers! but it does NOT own its 'right ledger'. nobody owns that ledger,
otherwise leftwards actors could have a say in the content of the right ledger. *)

(* any = pos | prov | other *)
(* box = pos | prov *)



let construct =

  let rec new_pos s =
    let* pos = create_user s in
    let* box = create_user (s^".box") in
    map_set boxes pos box >>
    return pos

  and set_owner pos owner = map_set owners pos owner

  and init_tl' source_name target_name contract =
    let* owner = get_caller in
    let* source = new_pos source_name in
    let* target = new_pos target_name in
    data_update sources (fun s -> SP.add s source) >>
    set_owner source owner >>
    set_owner target owner >>
    map_set nexts source target >>
    map_set segments source contract >>
    return (source,target)

  and legal_call (source,target) =
    let* caller = get_caller in
    map_find segments source >>= (function
        | Some segment_address ->
          if segment_address <> caller
          then error "caller is not segment of source"
          else map_find nexts source >>= (function
              | Some actual_target ->
                if actual_target <> target
                then error "target is not next of source"
                else return ()
              | None -> error "No next for position source")
        | None -> error "No segment for position source")

  (*and transfer_token_from (giver,tk,a,taker) =*)
    (*Ledger.transfer ledger giver tk a taker*)

  and transfer_token' tk a taker  =
    let* giver = get_caller in
    Ledger.transfer ledger giver tk a taker
    (*transfer_token_from (giver,tk,a,taker)*)

  and transfer_address_from giver addr taker  =
    let* addr_owner = map_find_exn owners addr in
    if addr_owner = giver
    then map_set owners addr taker
    else error "Cannot zerocross addresses, FIXME"

  and transfer_address' addr taker =
    let* giver = map_find_exn owners addr in
    transfer_address_from giver addr taker


(* given source,
   if in position
   source---(segment)---target, return Some (segment,target)
   if in position
   source---X, return None *)
  and sell_side source =
    let* target_opt = map_find nexts source in
    let* segment_opt = map_find segments source in
    match (target_opt,segment_opt) with
    | Some target, Some segment -> return (Some (target,segment))
    | None, None -> return None
    | _,_ -> error "Dec inconsistency: pos has one of (next,segment) but not the other"

  and grow' ((source,target),pos_name,owner,contract) =
    (* no need to check whether target is dead
       since a dead pos is next of nobody *)
    legal_call (source,target) >>
    map_find nexts target >>= function
    | Some _ ->
      error "Cannot grow a non-head of tradeline"
    | None ->
      let* pos = new_pos pos_name in
      map_set owners pos owner >>
      map_set nexts source pos >>
      map_set segments source contract >>
      return pos

  and pull' (source,target) =
    legal_call (source,target) >>
    (sell_side target >>= function
      | Some (new_target,_new_segment) ->
        (* target itself has a target:
           - connect source to new target, but keep segment *)
        map_set nexts source new_target
      | None ->
        (* target is the tradeline head:
           - disconnect source from target *)
        map_remove nexts source >>
        (* - disconnect source from segment *)
        map_remove segments source) >>
    (* eject target from tradeline structure *)
    map_remove nexts target >>
    map_remove segments target >>
    (* mark target as dead *)
    data_update deads (fun s -> SP.add s target)
  (* no position ownership transfer *)

  and commit' (source,target) =
    legal_call (source,target) >>
    (sell_side target >>= (function
         | Some (new_target, new_segment) ->
           (* target itself has a target:
              - connect source to new target *)
           map_set nexts source new_target >>
           (* - connect source to new segment *)
           map_set segments source new_segment
         | None ->
           (* target is the tradeline head:
              - disconnect source from target *)
           map_remove nexts source >>
           (* - disconnect source from segment *)
           map_remove segments source)) >>
    (* eject target from tradeline strcture *)
    map_remove nexts target >>
    map_remove segments target >>
    let* source_owner = map_find_exn owners source in
    let* source_box = map_find_exn boxes source in
    let* target_owner = map_find_exn owners target in
    let* target_box = map_find_exn boxes target in
    (* box of v is now box of u *)
    map_set boxes source target_box >>
    (* remove box of v *)
    map_remove boxes target >>
    (* box of u has been replaced and is now orphaned, give it to source_owner *)
    map_set owners source_box source_owner >>
    (* mark target as dead, so target_owner can collect *)
    data_update deads (fun s -> SP.add s target) >>
    (* give source to owner of target *)
    set_owner source target_owner

  and require_collectable addr =
    (* if not dead but no box, then it _is_ a box *)
    let* box_opt = map_find boxes addr in
    let* source = data_get sources >>= fun s -> return (SP.mem s addr) in
    let* dead = data_get deads >>= fun s -> return (SP.mem s addr) in
    let* next = map_find nexts addr in
    let not_pos = (not dead) && (box_opt = None) in
    let singleton = source && next = None in
    if not_pos || dead || singleton
    then return ()
    else error "Not collectable"

  and collect_token' giver tk =
    require_collectable giver >>
    (* need to add pos to deads ?*)
    let* owner = map_find_exn owners giver in
    Ledger.transfer_all ledger giver tk owner

(* addr should be a pos or a box *)
  and collect_address' giver addr =
    require_collectable giver >>
    (* need to add pos to deads ?*)
    let* owner = map_find_exn owners giver in
    transfer_address_from giver addr owner

(* UNSAFE *)
  and pay' (seller,buyer) giver_side token amount taker =
    legal_call (seller,buyer) >>
    let* giver = map_find_exn owners (if giver_side = Right then buyer else seller) in
    Ledger.transfer ledger giver token amount taker

  and collect_box' giver =
    require_collectable giver >>
    map_find boxes giver >>= function
    | None -> return ()
    | Some b -> let* owner = map_find_exn owners giver in map_set owners b owner

  in
  (* UNSAFE *)
  code_set pay (fun (p,g,tk,a,t) -> pay' p g tk a t) >>
  code_set init_tl (fun (s,t,c) -> init_tl' s t c) >>
  code_set grow grow' >>
  code_set pull pull' >>
  code_set commit commit' >>
  code_set collect_token (fun (g,t) -> collect_token' g t) >>
  code_set collect_address (fun (g,a) -> collect_address' g a) >>
  code_set owner_of (fun p -> map_find_exn owners p) >>
  code_set box_of (fun a -> map_find boxes a) >>
  code_set balance_of (fun (a,tk) -> Ledger.balance ledger a tk) >>
  code_set box_balance_of (fun (a,tk) -> callthis box_of a >>=
                            function None -> return 0 | Some b -> callthis balance_of (b,tk))
  >>
  code_set transfer_token (fun (tk,a,t) -> transfer_token' tk a t) >>
  code_set transfer_address (fun (g,t) -> transfer_address' g t) >>
  code_set collect_box collect_box' >>
  code_set Token.on_token_receive
    (fun (giver,token,amount) ->
       let* caller = get_caller in
       if caller = token then
         Ledger.add ledger giver token amount
       else
         return ())
