open Tools
open Imperative.P


(** Syntactic sugar *)
module MP = MP
module SP = SP
module A = Address

type amount = int

type token = A.t
[@@deriving show]

type pos = A.t
[@@deriving show]

(** Generic representation for choosing which side of a tradeline pos/segment we're talking about. *)
type side = Source | Target
[@@deriving show]

type parties = pos*pos

(*  <key>    = data ~pp:<printing fn>     <display name> *)
let ledger   = data ~pp:Ledger.pp         "ledger"
let owners   = data ~pp:(MP.pp A.pp A.pp) "owners"
let nexts    = data ~pp:(MP.pp A.pp A.pp) "nexts"
let segments = data ~pp:(MP.pp A.pp A.pp) "segments"
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
    let addr_owner = map_find_exn owners addr in
    if addr_owner = giver
    then transfer_address addr taker
    else error "Cannot zerocross addresses, FIXME"

  let set_owner pos owner = map_set owners pos owner
end

module User = struct
  let segment_of        : (A.t,A.t option) code_hkey = code ()
  (* Start a new tl with 2 positions *)
  let init_tl           : (string * string * A.t, A.t * A.t) code_hkey = code ()
  (* Transfers *)
  (* Give all tokens owned by address at some index to owner of address *)
  let collect_token     : (A.t * token, unit) code_hkey = code ()
  (* Give address1 owned by address2 to owner of address 2 *)
  let collect_address   : (A.t, unit) code_hkey = code ()
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
  (* convenience *)
  let box_balance_of    : (A.t * token, amount) code_hkey = code ()
  let fund_with_token   : (token * amount * pos * side,unit) code_hkey = code ()
  let fund_with_address : (A.t * pos * side,unit) code_hkey = code ()

  let new_pos s =
    let pos     = create_empty_contract s in
    let pos_box = create_empty_contract (s^".box") in
    map_set nexts pos pos_box ;
    return (pos,pos_box)

  let construct () =
    (* Dec *)

    (* get contract to left of pos, and to right of box *)
    (* could be non for first pos, could be none for last box *)
    code_set segment_of begin fun addr ->
      match map_find segments addr with
      | None ->
        let addr_box = map_find_exn nexts addr in
        map_find segments addr_box
      | Some _ ->
        match map_find nexts addr with
        | None -> return None
        | Some pos ->
          let pos_box = map_find_exn nexts pos in
          (* it would be invalid to return None here *)
          map_find segments pos_box
    end;


    code_set init_tl
      begin fun (source_name,target_name,contract) ->
        let owner = get_caller () in
        let (source,source_box) = new_pos source_name in
        let (target,target_box) = new_pos target_name in
        Admin.set_owner source owner ;
        Admin.set_owner target owner ;
        map_set nexts source_box target ;
        map_set segments target_box contract ;
        return (source,target)
      end ;


    code_set transfer_token
      begin fun (tk,a,taker) ->
        let giver = get_caller () in
        Admin.transfer_token_from giver tk a taker
      end ;

    code_set fund_with_token
      begin fun (token, amount, pos, side) ->
        let taker = match side with
          | Target -> return pos
          | Source ->
            match callthis box_of pos with
            | None ->
              error "this pos has no box"
            | Some box ->
              return box
              (* use transfer_*_from to check that giver is owner of addr *)
        in callthis transfer_token (token,amount,taker)
      end ;

    code_set transfer_address
      begin fun (addr,taker) ->
        let giver = get_caller () in
        Admin.transfer_address_from giver addr taker
      end ;

    code_set fund_with_address
      begin fun (addr, pos, side) ->
        let taker = match side with
          | Target -> return pos
          | Source ->
            match callthis box_of pos with
            | None -> error "this pos has no box"
            | Some box -> return box
            (* use transfer_*_from to check that giver is owner of addr *)
        in callthis transfer_address (addr,taker)
      end ;

    let test_collectable addr =
      let next_opt = map_find nexts addr in
      let segment_opt = map_find segments addr in
      return ((next_opt = None) && (segment_opt = None)) in

    let require_collectable addr =
      let collectable = test_collectable addr in
      if collectable
      then return ()
      else error "Not collectable" in

    code_set collect_token
      begin fun (giver, tk) ->
        require_collectable giver ;
        let owner = map_find_exn owners giver in
        Ledger.transfer_all ledger giver tk owner
      end ;

    code_set collect_address
      begin fun addr ->
        let giver = map_find_exn owners addr in
        require_collectable giver ;
        let taker = map_find_exn owners giver in
        Admin.transfer_address addr taker
      end ;

    code_set owner_of
      begin fun p ->
        map_find_exn owners p
      end ;

    code_set balance_of
      begin fun (a,tk) ->
        Ledger.balance ledger a ~index:"" tk
      end ;

    code_set box_balance_of
      begin fun (a,tk) ->
        match callthis box_of a with
        None -> return 0 | Some b -> callthis balance_of (b,tk)
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

  let construct () =

    let require_legal (source,target) =
      let source_box = map_find_exns "No box for source" nexts source in
      let target' = map_find_exns "No next for source box" nexts source_box in
      if target <> target'
      then error "target is not next of source box"
      else
        let target_box = map_find_exns "No box for target" nexts target in
        let segment = map_find_exns "No segment for target_box" segments target_box in
        let caller = get_caller () in
        if segment <> caller
        then error "caller is not segment of source"
        else return () in

    code_set grow
      begin fun ((source,target),pos_name,owner,contract) ->
        require_legal (source,target) ;
        let target_box = map_find_exn nexts target in
        match map_find nexts target_box with
        | Some _ ->
          error "Cannot grow a non-head of tradeline"
        | None ->
          let (pos,pos_box) = User.new_pos pos_name in
          map_set owners pos owner ;
          map_set nexts target_box pos ;
          map_set segments pos_box contract ;
          return pos
      end ;

    code_set pull
      begin fun (source,target) ->
        require_legal (source,target) ;
        let source_box = map_find_exn nexts source in
        let target_box = map_find_exn nexts target in
        let segment = map_find_exn segments target_box in
        (match map_find nexts target_box with
          | Some new_target ->
            let new_target_box = map_find_exn nexts new_target in
            map_set nexts source_box new_target ;
            map_set segments new_target_box segment
          | None ->
            map_remove nexts source) ;
        map_remove nexts target ;
        map_remove nexts target_box ;
        map_remove segments target_box
      end ;

    code_set commit
      begin fun (source,target) ->
        require_legal (source,target) ;
        let source_box = map_find_exn nexts source in
        let target_box = map_find_exn nexts target in
        (match map_find segments source_box with
          | Some segment ->
            map_set segments target_box segment ;
            map_remove segments source_box
          | None ->
            map_remove segments target_box) ;
        map_remove nexts source_box ;
        map_remove nexts target ;
        map_set nexts source target_box
      end ;

    code_set transfer_token
      begin fun (((source,target) as parties),side,tk,amount,taker) ->
        require_legal parties ;
        match side with
        | Source ->
          let source_box = map_find_exn nexts source in
          Admin.transfer_token_from source_box tk amount taker
        | Target ->
          Admin.transfer_token_from target     tk amount taker
      end ;

    code_set transfer_address
      begin fun (((source,target) as parties),side,address,taker) ->
        require_legal parties ;
        match side with
        | Source ->
          let source_box = map_find_exn nexts source in
          Admin.transfer_address_from source_box address taker
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
        proxy zwrap_proxy ~caller (fun () ->
          let dec = data_get dec_addr in
          let caller' = get_caller () in
          call dec enable () ;
          let retval = call caller' key (caller,args) in
          call dec disable () ;
          return retval
        )
    end
  end


  let construct () =
    let zwrap_proxy =
      let this = get_this () in
      create_contract "dec.zwrap_proxy" Proxy.construct this in

    code_set get_proxy
      begin fun () ->
        return zwrap_proxy
      end ;

    code_set enable
      begin fun () ->
        Ledger.zwrap_start ledger
      end ;

    code_set disable
      begin fun () ->
        let caller = get_caller () in
        require (return (caller = zwrap_proxy)) ;
        Ledger.zwrap_end ledger
      end ;

    code_set test
      begin fun () ->
        Ledger.is_zwrapping ledger
      end
end

let construct () =
  data_set ledger Ledger.empty ;
  data_set owners MP.empty ;
  data_set nexts MP.empty ;
  data_set segments MP.empty ;

  Legal.construct () ;
  Zwrap.construct () ;
  User.construct () ;

  (* Callbacks *)
  code_set Token.on_token_receive
    begin fun (giver,token,amount) ->
      let caller = get_caller () in
      if caller = token then
        Ledger.add ledger giver token amount
      else
        return ()
    end

let echo_dec =
  echo_data ledger ;
  echo_data owners ;
  echo_data nexts ;
  echo_data segments
