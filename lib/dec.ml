open Tools
open Env
(** Syntactic sugar *)
module MP = MP
module SP = SP

type token = Address.t
[@@deriving show]

type amount = int
(*[@@deriving show]*)

type pos = int*string (*Special NFT for positions*)
let pp_pos fmt (_,s) = F.p fmt "⦗%s⦘" s

(** Generic representation for choosing which side of a tradeline segment we're talking about. *)
type side = Seller | Buyer
[@@deriving show]

(** Ledger entries can represent either users or positions *)
and entry = Eaddr of Address.t | Epos of pos

let pp_entry fmt = function Eaddr a -> Address.pp fmt a | Epos p -> pp_pos fmt p
[@@ocaml.warning "-27"]

module Ledger =
struct
  type t = {map : ((entry * token),amount) MP.t ; z_crossings : int}

  let find hk k =
    let* l = data_get hk in
    return (MP.find l.map k)

  let find_exn hk k =
    find hk k >>= function
    | Some e -> return e
    | None -> error "not found"
  [@@ocaml.warning "-32"]

  let balance hk entry tk =
    let* l = data_get hk in
    return (MP.find l.map (entry,tk) |? 0)

  let add hk entry tk a =
    let* v = balance hk entry tk in
    data_update hk (fun l ->
        let z = l.z_crossings in
        let modifier = if (a+v<0 && v>=0) then 1 else if (a+v>=0 && v<0) then (-1) else 0 in
        {map = MP.set l.map (entry,tk) (a+v);z_crossings = (z+modifier)})

  let transfer hk entry1 entry2 tk a =
    add hk entry1 tk (-a) >> add hk entry2 tk a

  let transfer_up_to hk entry1 entry2 tk a =
    let* b = balance hk entry1 tk in
    transfer hk entry1 entry2 tk (min a b)

  let transfer_all hk entry1 entry2 tk =
    let* b = balance hk entry1 tk in
    transfer hk entry1 entry2 tk b

  let solvent hk =
    let* {z_crossings;_} = data_get hk in
    return (z_crossings = 0)

  let pp fmt l =
    F.p fmt "(%d zcrossings)" l.z_crossings;
    let printer fmt (e,t) a = F.cr (); F.p fmt "%a has %d%a" pp_entry e a pp_token t in
    MP.pp_i fmt printer l.map

  let empty = { map = MP.empty; z_crossings = 0; }

end

(*let pos_names : (pos,string) MP.t data_hkey*)
  (*= data ~init:MP.empty                                    "position names"*)
let ledger    = data ~pp:Ledger.pp                 ~init:Ledger.empty  "ledger"
let owners    = data ~pp:(MP.pp pp_pos pp_entry)   ~init:MP.empty      "owners"
let sources   = data ~pp:(SP.pp pp_pos)            ~init:SP.empty      "sources"
let nexts     = data ~pp:(MP.pp pp_pos pp_pos)     ~init:MP.empty      "nexts"
let segments  = data ~pp:(MP.pp pp_pos Address.pp) ~init:MP.empty      "segments"
let deads     = data ~pp:(SP.pp pp_pos)            ~init:SP.empty      "deads"
let max_pos   = data ~pp:Format.pp_print_int       ~init:0             "max_pos"

let print_dec =
  echo_data ledger >>
  echo_data owners >>
  echo_data sources >>
  echo_data nexts >>
  echo_data segments >>
  echo_data deads >>
  echo_data max_pos

type parties = pos*pos

(* Change tl topology *)
let init_tl : (string*string*Address.t,pos*pos) code_hkey = code ()
let grow : (parties * string * entry * Address.t, pos) code_hkey = code ()
let pull : (parties,unit) code_hkey = code ()
let commit  : (parties,unit) code_hkey = code ()
(* Transfers *)
let pay : (parties * side * entry * token * amount,unit) code_hkey = code ()
let draw_up_to : (parties * token * amount, unit) code_hkey = code ()
let collect_token : (pos * token,unit) code_hkey = code ()
let collect_pos : (pos * pos,unit) code_hkey = code ()
(* Reading *)
let owner_of : (pos,entry) code_hkey = code ()
let get_provision : (parties * token,amount) code_hkey = code ()
let provision_hte : (parties * entry * token * amount,bool) code_hkey = code ()
let provision_lt : (parties * entry * token * amount,bool) code_hkey = code ()

let transfer_token_to_provision : (token*amount*pos,unit) code_hkey = code ()
let transfer_pos_to_provision : (pos*pos,unit) code_hkey = code ()
let transfer_token : (token*amount*Address.t,unit) code_hkey = code ()
let transfer_pos : (pos*Address.t,unit) code_hkey = code ()

let owner_of' (p:pos) = map_find_exn owners p

let construct =

  let rec new_pos s = data_get max_pos >>= fun p -> data_set max_pos (p+1) >> return (p,s)

  and set_owner pos owner = map_set owners pos owner

  and init_tl' (seller_name,buyer_name,contract) =
    let* owner' = get_caller in
    let owner = Eaddr owner' in
    let* seller = new_pos seller_name in
    let* buyer = new_pos buyer_name in
    data_update sources (fun s -> SP.add s seller) >>
    set_owner seller owner >>
    set_owner buyer owner >>
    map_set nexts seller buyer >>
    map_set segments seller contract >>
    return (seller,buyer)


  and legal_call (seller,buyer) =
    get_caller >>= fun caller ->
    map_find segments seller >>= (function
        | Some segment_address ->
          if segment_address <> caller
          then error "caller is not segment of seller"
          else map_find nexts seller >>= (function
              | Some actual_buyer ->
                if actual_buyer <> buyer
                then error "buyer is not next of seller"
                else return ()
              | None -> error "No next for position seller")
        | None -> error "No segment for position seller")

  and pay' ((seller,buyer),side_from,entry_to,token,amount) =
    legal_call (seller,buyer) >>
    let* entry_from = if side_from = Buyer then owner_of' buyer else owner_of' seller in
    Ledger.transfer ledger entry_from entry_to token amount

  and draw_up_to' ((seller,buyer),tk,amount) =
    legal_call (seller,buyer) >>
    let* seller_owner = owner_of' seller in
    Ledger.transfer ledger (Epos buyer) seller_owner tk amount

  and get_provision' ((seller,_buyer),tk) =
    (* public information *)
    Ledger.balance ledger (Epos seller) tk

  and provision_hte' ((_seller,_buyer),who,tk,a) =
    (* public information *)
    let* b = Ledger.balance ledger who tk in return (b >= a)

  and provision_lt' ((_seller,_buyer),who,tk,a) =
    (* public information *)
    let* b = Ledger.balance ledger who tk in return (b < a)

  and sell_side seller =
    let* buyer_opt = map_find nexts seller in
    let* segment_opt = map_find segments seller in
    match (buyer_opt,segment_opt) with
    | Some buyer, Some segment -> return (Some (buyer,segment))
    | None, None -> return None
    | _,_ -> error "Dec inconsistency: pos has one of (next,segment) but not the other"

  and grow' ((seller,buyer),pos_name,owner,contract) =
    (* no need to check whether buyer is dead
       since a dead pos is next of nobody *)
    legal_call (seller,buyer) >>
    map_find nexts buyer >>= function
    | Some _ -> error "Cannot grow a non-head of tradeline"
    | None ->
      let* pos = new_pos pos_name in
      map_set owners pos owner >>
      map_set nexts seller pos >>
      map_set segments seller contract >>
      return pos

  and pull' (seller,buyer) =
    legal_call (seller,buyer) >>
    (sell_side buyer >>= function
      | Some (new_buyer,_new_segment) ->
        (* buyer itself has a buyer:
           - connect seller to new buyer, but keep segment *)
        map_set nexts seller new_buyer
      | None ->
        (* buyer is the tradeline head:
           - disconnect seller from buyer *)
        map_remove nexts seller >>
        (* - disconnect seller from segment *)
        map_remove segments seller) >>
    (* eject buyer from tradeline structure *)
    map_remove nexts buyer >>
    map_remove segments buyer >>
    (* mark buyer as dead *)
    data_update deads (fun s -> SP.add s buyer)
  (* no position ownership transfer *)

  and commit' (seller,buyer) =
    legal_call (seller,buyer) >>
    (sell_side buyer >>= (function
         | Some (new_buyer, new_segment) ->
           (* buyer itself has a buyer:
              - connect seller to new buyer *)
           map_set nexts seller new_buyer >>
           (* - connect seller to new segment *)
           map_set segments seller new_segment
         | None ->
           (* buyer is the tradeline head:
              - disconnect seller from buyer *)
           map_remove nexts seller >>
           (* - disconnect seller from segment *)
           map_remove segments seller)) >>
    (* eject buyer from tradeline strcture *)
    map_remove nexts buyer >>
    map_remove segments buyer >>
    (* mark buyer as dead *)
    data_update deads (fun s -> SP.add s buyer) >>
    (* give seller to owner of buyer *)
    let* buyer_owner = owner_of' buyer in
    set_owner seller buyer_owner

  and require_singleton pos =
    let* source = data_get sources >>= fun s -> return (SP.mem s pos) in
    let* dead = data_get deads >>= fun s -> return (SP.mem s pos) in
    let* next = map_find nexts pos in
    if dead || (source && next = None)
    then return ()
    else error "Not a singleton"

  and collect_token' (pos,t) =
    require_singleton pos >>
    (* need to add pos to deads ?*)
    let* owner = owner_of' pos in
    Ledger.transfer_all ledger (Epos pos) owner t

  and collect_pos' (pos,pos') =
    require_singleton pos >>
    (* need to add pos to deads ?*)
    let* owner = owner_of' pos in
    map_set owners pos' owner in

  let transfer_token_any tk a taker_entry =
    let* giver = get_caller in
    Ledger.transfer ledger (Eaddr giver) taker_entry tk a in

  let transfer_pos_any given taker_entry =
    let* caller = get_caller in
    let* giver = owner_of' given in
    if (Eaddr caller) = giver then
      set_owner given taker_entry
    else
      error "Cannot give position you do not own" in

  code_set init_tl init_tl' >>
  code_set grow grow' >>
  code_set pull pull' >>
  code_set commit commit' >>
  code_set pay pay' >>
  code_set draw_up_to draw_up_to' >>
  code_set collect_token collect_token' >>
  code_set collect_pos collect_pos' >>
  code_set owner_of owner_of' >>
  code_set get_provision get_provision' >>
  code_set provision_hte provision_hte' >>
  code_set provision_lt provision_lt' >>
  code_set transfer_token_to_provision
    (fun (tk,a,taker) -> transfer_token_any tk a (Epos taker)) >>
  code_set transfer_pos_to_provision
    (fun (given,taker) -> transfer_pos_any given (Epos taker)) >>
  code_set transfer_token
    (fun (tk,a,taker) -> transfer_token_any tk a (Eaddr taker)) >>
  code_set transfer_pos
    (fun (given,taker) -> transfer_pos_any given (Eaddr taker)) >>
  code_set Token.on_token_receive
    (fun (giver,token,amount) ->
       let* caller = get_caller in
       if caller = token then
         Ledger.add ledger (Eaddr giver) token amount
       else
         return ())
