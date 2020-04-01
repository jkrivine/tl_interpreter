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
  type t = {map : ((A.t * string * token),amount) MP.t ; z_crossings : int; z_nestings: int}

  let solvent hk =
    let* {z_crossings;_} = data_get hk in
    return (z_crossings = 0)

  let z_nesting_incr hk =
    data_update hk (fun l -> {l with z_nestings = l.z_nestings + 1})

  let z_nesting_decr hk =
    let* l = data_get hk in
    let l' = {l with z_nestings = l.z_nestings - 1 } in
    data_set hk l' >>
    if l'.z_nestings = 0 then begin
      let* is_solvent = solvent hk in
      if is_solvent
      then return ()
      else error "ledger is not solvent"
    end
    else
      return ()

  let z_protect hk e =
       z_nesting_incr hk
    >> e
    >> z_nesting_decr hk

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
        let z = l.z_crossings in
        let modifier = if (a+v<0 && v>=0) then 1 else if (a+v>=0 && v<0) then (-1) else 0 in
        {l with map = MP.set l.map (who,index,tk) (a+v);z_crossings = (z+modifier)})

  let transfer hk (giver:A.t) ?(index="") (tk:token) (a:amount) (taker:A.t) =
    z_protect hk begin
      add hk giver ~index tk (-a) >> add hk taker ~index tk a
    end

  let transfer_up_to hk (giver:A.t) ?(index="") (tk:token) (a:amount) (taker:A.t) =
    let* b = balance hk giver ~index tk in
    transfer hk giver ~index tk (min a b) taker

  let transfer_all hk (giver:A.t) ?(index="") (tk:token) (taker:A.t) =
    let* a = balance hk giver ~index tk in
    transfer hk giver ~index tk a taker

  let pp fmt l =
    F.p fmt "(%d zcrossings, %d znestings)" l.z_crossings l.z_nestings;
    let printer fmt (addr,index,t) amount = F.cr (); F.p fmt "%a.%s has %d%a" A.pp addr index amount pp_token t in
    MP.pp_i fmt printer l.map

  let empty = { map = MP.empty; z_crossings = 0; z_nestings = 0}
end

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
let collect_token_indexed : (A.t * string * token, unit) code_hkey = code ()
let collect_address : (A.t * A.t, unit) code_hkey = code ()
let collect_box : (A.t, unit) code_hkey = code ()
let transfer_token : (token * amount * A.t, unit) code_hkey = code ()
let transfer_token_indexed : (string * token * amount * A.t, unit) code_hkey = code ()
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
let balance_of_indexed : (A.t * string * token, amount) code_hkey = code ()
(* Convenience composition of right_prov and get_balance *)
let box_balance_of : (A.t * token, amount) code_hkey = code ()
(* Obj.magic going on here *)
let z_protect : (A.t * (unit,unit) code_hkey,unit) code_hkey = code ()


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

module Magic = struct
  (* FIXME: Not secure. The call should go through an additional 'proxy'
     contract with any identity other than `dec`. Otherwise, as is we can ask
     Dec to transfer money to anyone.

     The following implements bouncing through `dec` to surround a call with
     z-crossing parentheses.  We do it in an especially ugly way because, in
     this OCaml version, we must deal with type safety. Since `z_protect` is a
     stored procedure, it cannot be polymorphic (As a ref can only be weakly
     polymorphic. There may be workarounds I'm not aware of.), so it cannot
     take any `('a,'b) code_hkey` as argument (to wrap the execution of the
     hkey in a z_(incr/decr)). We get around this limitation in a way which
     would also work in Solidity: we define two variables `_in`, `_out` (in
     Solidity: getter/setters), `_in` for input data, which is set before
     calling the function. That function sets `_out` (for output data) and
     returns unit.  Thus z_protect can have type `((unit,unit) code_hkey,unit)
     code_hkey`.

     If a segment `s` wants to initiate a call, it may get z-crossing by
     wrapping its code between

     ``` z_nesting_incr <code> z_nesting_decr ```

     However Dec cannot accept any public call to `z_nesting_incr` otherwise
     the nesting may not end with a solvency check, and a transaction could
     succeed with Dec still insolvent.

     This is fixed by `s` giving an hkey `hk` to `Dec` which `Dec` calls :

     ``` z_nesting_incr call s hkey () z_nesting decr ```

     Now `Dec` can safely know that whatever happens in the call, the
     transaction will not succeed without a successful solvency check.

     The above is unsatisfactory for 2 reasons: 1) Dec is calling arbitrary
     code with its own identity 2) If a method from `s` requires a caller
     identity check (eg only user u can trigger forward), the above scheme
     breaks.

     To fix 1), Dec should have a proxy `p` which implements the
     z_nesting_(incr/decr) wrapping. to fix 2), that proxy `p` should pass a
     caller argument to the segment `s`, which `s` should know it can trust.
  *)
  let z_protect_code_set dec code_hkey commands =
    let in_args = Env.data_hidden ()
    and out_args = Env.data_hidden ()
    and internal_hkey = Env.code () in
    let* this  = get_this in
    code_set internal_hkey (fun () ->
        let* args = data_get in_args in
        let* ret = commands args in
        data_set out_args ret) >>
    code_set code_hkey (fun args ->
        data_set in_args args >>
        Env.call dec z_protect (this,internal_hkey)
        >> data_get out_args)
end

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

  and transfer_token' ?(index="") tk a taker  =
    let* giver = get_caller in
    Ledger.transfer ledger giver ~index tk a taker
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

  and collect_token' giver ?(index="") tk =
    require_collectable giver >>
    (* need to add pos to deads ?*)
    let* owner = map_find_exn owners giver in
    Ledger.transfer_all ledger giver ~index tk owner

  (* addr should be a pos or a box *)
  and collect_address' giver addr =
    require_collectable giver >>
    (* need to add pos to deads ?*)
    let* owner = map_find_exn owners giver in
    transfer_address_from giver addr owner

  (* UNSAFE *)
  and pay' (source,target) giver_side ?(index="") token amount taker =
    legal_call (source,target) >>
    let* giver = map_find_exn owners (if giver_side = Right then target else source) in
    Ledger.transfer ledger giver ~index token amount taker

  and collect_box' giver =
    require_collectable giver >>
    map_find boxes giver >>= function
    | None -> return ()
    | Some b -> let* owner = map_find_exn owners giver in map_set owners b owner

  (* This feels very dangerous. Is it OK? yes as long as protection occurs through yet
     another contract. *)
  and z_protect' (address,hkey) =
    Ledger.z_protect ledger begin
      call address hkey ()
    end



  in
  (* UNSAFE *)
  code_set pay (fun (p,g,tk,a,t) -> pay' p g ~index:"" tk a t) >>
  code_set init_tl (fun (s,t,c) -> init_tl' s t c) >>
  code_set grow grow' >>
  code_set pull pull' >>
  code_set commit commit' >>
  code_set collect_token (fun (g,t) -> collect_token' g ~index:"" t) >>
  code_set collect_token_indexed (fun (g,index,t) -> collect_token' g ~index t) >>
  code_set collect_address (fun (g,a) -> collect_address' g a) >>
  code_set owner_of (fun p -> map_find_exn owners p) >>
  code_set box_of (fun a -> map_find boxes a) >>
  code_set balance_of (fun (a,tk) -> Ledger.balance ledger a ~index:"" tk) >>
  code_set balance_of_indexed (fun (a,index,tk) -> Ledger.balance ledger a ~index tk) >>
  code_set box_balance_of (fun (a,tk) -> callthis box_of a >>=
                            function None -> return 0 | Some b -> callthis balance_of (b,tk))
  >>
  code_set z_protect z_protect'
  >>
  code_set transfer_token (fun (tk,a,t) -> transfer_token' ~index:"" tk a t) >>
  code_set transfer_token_indexed (fun (index,tk,a,t) -> transfer_token' ~index tk a t) >>
  code_set transfer_address (fun (g,t) -> transfer_address' g t) >>
  code_set collect_box collect_box' >>
  code_set Token.on_token_receive
    (fun (giver,token,amount) ->
       let* caller = get_caller in
       if caller = token then
         Ledger.add ledger giver ~index:"" token amount
       else
         return ())
