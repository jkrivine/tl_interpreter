open Tools
open Env


(** Syntactic sugar *)
module MP = MP
module SP = SP
module A = Address

type amount = int

type token = A.t
[@@deriving show]

type pos = A.t
[@@deriving show]

(** Generic representation for choosing which side of a tradeline segment we're talking about. *)
type side = Source | Target
[@@deriving show]

type parties = pos*pos

(*  <key>    = data ~pp:<printing fn>     ~init:<init value> <display name> *)
let ledger   = data ~pp:Ledger.pp         ~init:Ledger.empty "ledger"
let owners   = data ~pp:(MP.pp A.pp A.pp) ~init:MP.empty     "owners"
let sources  = data ~pp:(SP.pp A.pp)      ~init:SP.empty     "sources"
let nexts    = data ~pp:(MP.pp A.pp A.pp) ~init:MP.empty     "nexts"
let segments = data ~pp:(MP.pp A.pp A.pp) ~init:MP.empty     "segments"
let deads    = data ~pp:(SP.pp A.pp)      ~init:SP.empty     "deads"
let boxes    = data ~pp:(MP.pp A.pp A.pp) ~init:MP.empty     "boxes"
(* Positions have two 'boxes', a forward one and a backward one.
   The backward one is tied to the position itself. It cannot be detached.
   The forward one is called its 'box'.
   A box can be owned by anyone: a position, a user, another box, a contract...
   But boxes b come into existence owned by nobody, and attached as the box of a position p. Morally, b is the 'forward box' of p. This does *not* mean that p owns b. In that case, b would morally be in the 'backward box' of p. *)


(* This is just a module to separate admin code that
   should not be called from the outside *)
module Admin = struct
  let transfer_token_from giver tk a taker =
    Ledger.transfer ledger giver tk a taker

  let transfer_address addr taker =
    map_set owners addr taker

  let transfer_address_from giver addr taker  =
    let* addr_owner = map_find_exn owners addr in
    if addr_owner = giver
    then transfer_address addr taker
    else error "Cannot zerocross addresses, FIXME"

  let set_owner pos owner = map_set owners pos owner
end

module User = struct
  (* Start a new tl with 2 positions *)
  let init_tl           : (string * string * A.t, A.t * A.t) code_hkey = code ()
  (* Transfers *)
  (* Give all tokens owned by address at some index to owner of address *)
  let collect_token     : (A.t * token, unit) code_hkey = code ()
  (* Give address1 owned by address2 to owner of address 2 *)
  let collect_address   : (A.t, unit) code_hkey = code ()
  (* Detach box from pos and give it to pos owner. Argument is the pos *)
  let collect_box       : (A.t, unit) code_hkey = code ()
  (* Transfer token amount from caller to address *)
  let transfer_token    : (token * amount * A.t, unit) code_hkey = code ()
  (* Transfer address from caller to address *)
  let transfer_address  : (A.t * A.t, unit) code_hkey = code ()
  (* read info *)
  (* owners of boxes&positions are anything *)
  let owner_of          : (A.t, A.t) code_hkey = code ()
  (* a position may or may not have a box *)
  (* pos -> prov *)
  let box_of            : (A.t, A.t option) code_hkey = code ()
  (* any -> ... *)
  let balance_of        : (A.t * token, amount) code_hkey = code ()
  (* Convenience composition of right_prov and get_balance *)
  let box_balance_of    : (A.t * token, amount) code_hkey = code ()
  let fund_with_token   : (token * amount * pos * side,unit) code_hkey = code ()
  let fund_with_address : (A.t * pos * side,unit) code_hkey = code ()

  let new_pos s =
    let* pos = create_user s in
    let* box = create_user (s^".box") in
    map_set boxes pos box >>
    return pos

  let construct =
    (* Dec *)

    code_set init_tl
      begin fun (source_name,target_name,contract) ->
        let* owner = get_caller in
        let* source = new_pos source_name in
        let* target = new_pos target_name in
        data_update sources (fun s -> SP.add s source) >>
        Admin.set_owner source owner >>
        Admin.set_owner target owner >>
        map_set nexts source target >>
        map_set segments source contract >>
        return (source,target)
      end >>


    code_set transfer_token
      begin fun (tk,a,taker) ->
        let* giver = get_caller in
        Admin.transfer_token_from giver tk a taker
      end >>

    code_set fund_with_token
      begin fun (token, amount, pos, side) ->
        let* giver = get_caller in
        let* taker = match side with
          | Target -> return pos
          | Source ->
            callthis box_of pos >>= function
            | None ->
              error "this pos has no box"
            | Some box ->
              return box
              (* use transfer_*_from to check that giver is owner of addr *)
        in Admin.transfer_token_from giver token amount taker
      end >>

    code_set fund_with_address
      begin fun (addr, pos, side) ->
        let* giver = get_caller in
        let* taker = match side with
          | Target -> return pos
          | Source ->
            callthis box_of pos >>= function
            | None -> error "this pos has no box"
            | Some box -> return box
            (* use transfer_*_from to check that giver is owner of addr *)
        in Admin.transfer_address_from giver addr taker
      end >>

    let require_collectable addr =
      (* if not dead but has no box, then it _is_ a box *)
      let* box_opt = map_find boxes addr in
      let* source = data_get sources >>= fun s -> return (SP.mem s addr) in
      let* dead = data_get deads >>= fun s -> return (SP.mem s addr) in
      let* next = map_find nexts addr in
      let not_pos = (not dead) && (box_opt = None) in
      let singleton = source && next = None in
      if not_pos || dead || singleton
      then return ()
      else error "Not collectable" in

    code_set collect_token
      begin fun (giver, tk) ->
        require_collectable giver >>
        (* need to add pos to deads ?*)
        let* owner = map_find_exn owners giver in
        Ledger.transfer_all ledger giver tk owner
      end >>

    (* addr's owner should be a pos or a box *)
    code_set collect_address
      begin fun addr ->
        let* giver = map_find_exn owners addr in
        require_collectable giver >>
        (* need to add pos to deads ?*)
        let* taker = map_find_exn owners giver in
        Admin.transfer_address addr taker
      end >>

    code_set collect_box
      begin fun giver ->
        require_collectable giver >>
        map_find boxes giver >>= function
        | None ->
          return ()
        | Some b ->
          let* owner = map_find_exn owners giver
          in map_set owners b owner
      end >>

    code_set owner_of
      begin fun p ->
        map_find_exn owners p
      end >>

    code_set box_of
      begin fun a ->
        map_find boxes a
      end >>

    code_set balance_of
      begin fun (a,tk) ->
        Ledger.balance ledger a ~index:"" tk
      end >>

    code_set box_balance_of
      begin fun (a,tk) ->
        callthis box_of a >>=
        function None -> return 0 | Some b -> callthis balance_of (b,tk)
      end
end
(* Contract will be imported into Dec *)
module Legal = struct
  (* Extend a tl *)
  let grow             : (parties * string * A.t * A.t, pos) code_hkey = code ()
  (* Backward reduce *)
  let pull             : (parties, unit) code_hkey = code ()
  (* Forward reduce *)
  let commit           : (parties, unit) code_hkey = code ()
  (* Send tokens from left or right ledger to address *)
  let transfer_token   : (parties * side * token * amount * A.t,unit) code_hkey = code ()
  let transfer_address : (parties * side * A.t * A.t,unit) code_hkey = code ()

  let construct =

    let require_legal (source,target) =
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
          | None -> error "No segment for position source") in

    code_set grow
      begin fun ((source,target),pos_name,owner,contract) ->
        (* no need to check whether target is dead
           since a dead pos is next of nobody *)
        require_legal (source,target) >>
        map_find nexts target >>= function
        | Some _ ->
          error "Cannot grow a non-head of tradeline"
        | None ->
          let* pos = User.new_pos pos_name in
          map_set owners pos owner >>
          map_set nexts source pos >>
          map_set segments source contract >>
          return pos
      end >>

    (* given source,
       if in position source---(segment)---target, return Some (segment,target)
       if in position source---X, return None *)
    let sell_side source =
      let* target_opt = map_find nexts source in
      let* segment_opt = map_find segments source in
      match (target_opt,segment_opt) with
      | Some target, Some segment -> return (Some (target,segment))
      | None, None -> return None
      | _,_ -> error "Dec inconsistency: pos has one of (next,segment) but not the other" in

    code_set pull
      begin fun (source,target) ->
        require_legal (source,target) >>
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
      end >>

    code_set commit
      begin fun (source,target) ->
        require_legal (source,target) >>
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
        Admin.set_owner source target_owner
      end >>

    code_set transfer_token
      begin fun (((source,target) as parties),side,tk,amount,taker) ->
        require_legal parties >>
        match side with
        | Source ->
          let* box = map_find_exn boxes source in
          Admin.transfer_token_from box    tk amount taker
        | Target ->
          Admin.transfer_token_from target tk amount taker
      end >>

    code_set transfer_address
      begin fun (((source,target) as parties),side,address,taker) ->
        require_legal parties >>
        match side with
        | Source ->
          let* box = map_find_exn boxes source in
          Admin.transfer_address_from box address taker
        | Target ->
          Admin.transfer_address_from target address taker
      end
end

module Zwrap = struct
  let get_proxy : (unit,A.t)  code_hkey = code ()
  let enable    : (unit,unit) code_hkey = code ()
  let disable   : (unit,unit) code_hkey = code ()
  let test      : (unit,bool) code_hkey = code ()


  (* A contract with its own address, here for security reasons *)
  module Proxy = struct
    let dec_addr = data "dec"
    let construct dec =
      data_set dec_addr dec
    module Magic = struct
      (* Should be a command stored in an hkey *)
      let call_zwrap zwrap_proxy (caller,key,args) =
        Env.proxy zwrap_proxy ~caller (
          let* dec = data_get dec_addr in
          let* caller' = get_caller in
          call dec enable () >>
          call caller' key (caller,args) >>= fun retval ->
          call dec disable () >>
          return retval
        )
    end
  end


  let construct =
    let* (zwrap_proxy,()) =
      let* this = get_this in
      create_contract "dec.zwrap_proxy" (Proxy.construct this) in

    code_set get_proxy
      begin fun () ->
        return zwrap_proxy
      end >>

    code_set enable
      begin fun () ->
        Ledger.zwrap_start ledger
      end >>

    code_set disable
      begin fun () ->
        let* caller = get_caller in
        require (return (caller = zwrap_proxy)) >>
        Ledger.zwrap_end ledger
      end >>

    code_set test
      begin fun () ->
        Ledger.is_zwrapping ledger
      end
end

let construct =
  Legal.construct >>
  Zwrap.construct >>
  User.construct >>

  (* Callbacks *)
  code_set Token.on_token_receive
    begin fun (giver,token,amount) ->
      let* caller = get_caller in
      if caller = token then
        Ledger.add ledger giver token amount
      else
        return ()
    end

let echo_dec =
  echo_data ledger >>
  echo_data owners >>
  echo_data sources >>
  echo_data nexts >>
  echo_data segments >>
  echo_data deads >>
  echo_data boxes
