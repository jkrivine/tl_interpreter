(* Warning! The code below is super ugly. You probably want to wait until it
   has been cleaned it up before reading it. *)

open Env.Imp
module P = Program
module C = Chain
module A = Address


type dir = Pull | Commit
[@@deriving show { with_path = false }]



module Make(D:sig val d : Address.t end) = struct
  let dec = D.d
  open Program
  open Dec.User

  module Contractions = struct

    (*type t = (dir*int) list*)
    type t = (dir*Address.t*Address.t*Address.t) list

    (*let show l =*)
      (*String.concat "\n" @@ List.map (fun (dir,u,seg,v) ->*)
          (*match dir with*)
          (*| Pull -> Lines.backward (A.show u) (A.show seg) (A.show v)*)
          (*| Commit -> Lines.forward (A.show u) (A.show seg) (A.show v)) l*)
    [@@deriving show]

    type r = (Address.t*Address.t*Address.t) list list
    [@@deriving show]

    let iter tls callback =
      let rec cts acts stak = function
        | [] ->
          if stak = [] then
            callback (List.rev acts)
          else
            let kats = List.map (fun l -> List.rev l) (List.filter (fun l -> l <> []) stak) in
            cts acts [] kats
        | tl::tls ->
          match tl with
          | (u,s,v)::xs ->
            let xs' f = match xs with
              | [] -> []
              | (_,t,w)::ys -> (f t w)::ys in
            cts ((Pull,u,s,v)::acts) stak ((xs' (fun _ w -> (u,s,w)))::tls);
            cts ((Commit,u,s,v)::acts) stak ((xs' (fun t w -> (u,t,w)))::tls);
            if (xs <> [] || tls <> []) then
              let stak' =  match stak with
                | [] -> [[(u,s,v)]]
                | l::ls -> ((u,s,v)::l)::ls in
              cts acts stak' (xs::tls)
          | [] ->
            cts acts stak tls
      in
      cts [] [] tls

    let map f tls =
      let r = ref [] in
      iter tls (fun l -> r := l::!r);
      List.map f (List.rev !r)

    let to_list length = map (fun l -> l) length

  end

  let orig c = proxy dec (fun () -> map_find_exn Dec.origins c)
  let orig_opt c = proxy dec (fun () -> map_find Dec.origins c)

   let triples_tl ?f pos =
     let rec recurse pos =
       (match f with None -> () | Some f -> f pos);
       match call dec next_of pos with
       | None -> []
       | Some pos' -> let h = (pos,Option.get (call dec segment_of pos'),pos')
         in h::(recurse pos') in
   recurse pos

  let triples_tls' poss =

    let retval = ref []
    and pending = ref (List.map (fun p -> orig p) poss)
    and seen = ref [] in

    let record_owner pos =
    P.proxy dec (fun () ->
          let c = callthis owner_of pos in
          match map_find Dec.origins c with
          | None -> ()
          | Some o ->
            if not (List.mem o !pending || List.mem o !seen)
           then pending := o::!pending else ()) in

    let rec treat () =
      match !pending with
      | [] -> ()
      | p::pending' ->
        pending:=pending';
        seen:=p::!seen;
        retval :=
          (let cell = triples_tl ~f:(fun p -> record_owner p) p in
          (p,cell)::!retval);
        treat () in

    treat (); !retval


  let triples_tls poss =
    List.map (fun (_,l) -> l) (triples_tls' poss)


  let trail_tls poss =
    let rec r last = function [] -> last | (p,_,p')::xs -> p::(r [p'] xs) in
    List.map (fun (p,l) -> r [p] l) (triples_tls' poss)

  let trail_tl pos =
    let rec r last = function [] -> last | (p,_,p')::xs -> p::(r [p'] xs) in
    r [orig pos] (triples_tl pos)

  let rec simple_trail ?(prev=[]) = function
    | [] -> prev
    | (u,_,v)::xs -> u::(simple_trail ~prev:[v] xs)


  let owners_tl pos =
    List.map (fun pos -> call dec owner_of pos) (trail_tl pos)

  let owners_tls poss =
    let folder l ll =
      let mapped = List.map (fun pos -> call dec owner_of pos) ll in
      l@mapped
    in List.fold_left folder [] (trail_tls poss)

  let segments_tl pos = List.map (fun (_,s,_) -> s) (triples_tl pos)

  let show_tradelines poss =
    let to_s l = List.map Address.show l in
    let mapped = (triples_tls' poss) |>
                 List.map (fun (o,_) ->
                      Lines.manual
                        [Inter (to_s @@ owners_tl o);
                         Inter (to_s @@ trail_tl o);
                         Lines Default;
                         Flush (to_s @@ segments_tl o)]) in
    String.concat "\n" mapped

  let show_tradelines_with_contractions tlc =
    let to_s l = Address.show l in
    let mapped = tlc |>
                 List.map (fun l ->
                     let dirs = l |> List.map (fun (_,_,_,i,d) ->
                         F.fs "%i.%s" (i+1) (show_dir d)) in
                     (*F.s ([%show: string list] dirs);*)
                     let pos = l |> List.map (fun (u,s,v,_,_) -> (u,s,v)) |> simple_trail |>
                               List.map to_s
                     in
                     (*F.s ([%show: string list] pos);*)
                     let segs = l |> List.map (fun (_,s,_,_,_) -> to_s s) in
                     (*F.s ([%show: string list] segs);*)
                     Lines.manual
                       [Flush dirs;
                        Inter pos;
                        Lines Default;
                        Flush segs]) in
    String.concat "\n" mapped


  let crawl' times (dir,u,s,v) = begin
    let ident = dir |> function Commit -> Segment.commit | Pull -> Segment.pull in
    begin (* apply times *)
      match List.find_opt (fun ((_,_,t) as tup) -> tup = (s,dir,t)) times with
      | Some (_,_,t) ->
        let time = P.time_get () in
        if time > t then
          ()
        else (
          P.echo_pp "Setting time to %i\n" t;
          C.time_set t
        )
      | None -> ()
    end;
    let time = P.time_get () in

    let rec try_call t =
      if t < 1000 then (
        let prev_time = P.time_get () in
        C.time_set t;
        (*let time_reads = Env.time_reads () in*)
        call s ident (u,v);
        (*P.echo "************************";*)
        (*P.echo_state ();*)
        (* A way of checking if a reduction occured *)
        if call dec Dec.User.box_of v = None then begin
          (if t <> prev_time then P.echo_pp "Setting time to %i\n" t);
        end else (
          C.state_restore "try_call";
          try_call (t+1)
        )
      ) else (
        P.echo_pp "Cannot reduce with %i <= time < 1000.\n" time;
          P.error "cannot find good time split"
      ) in

    C.state_save "try_call";
    try_call time ;


    (*call s ident (u,v);*)
    if call dec Dec.User.is_singleton u then
    let owner = call dec owner_of u in
    C.tx owner dec free_singleton u;
  end

  (* Does not handle a player owning another player *)
  let rec up_owner_chain owned ulti_owner f =
    if owned = ulti_owner then () else (
      ignore (f owned);
      (match call dec owner_of_opt owned with
       | Some o' ->  up_owner_chain o' ulti_owner f
       | None -> ())
    )

  let rec first_in players owned =
    if List.mem owned players then Some owned else
      match call dec owner_of_opt owned with
      | Some o' -> first_in players o'
      | None -> None

  let collect players =
    P.proxy dec (fun () ->
        MP.iteri (P.data_get Dec.owners) @@ fun owned _ ->
        match first_in players owned with
        | None -> ()
        | Some first_owner ->
          up_owner_chain owned first_owner @@ fun owned ->
          Ledger.Offchain.iter_address_i Dec.ledger owned @@ fun token _ ->
          callthis collect_token (owned,token)
      )

  let rec next_nth p i =
    if i <= 0
    then p
    else next_nth (Option.get (call dec next_of p)) (i-1)

  (* poss is a list of positions.
     unroll will take into account all tradelines going right and up from the _origins_
     of those positions, recursively. It does not go down.
     So if a tradeline looks like

     u0------------------u1
    owns                owns
     u----v               u'-----v'

     you must give a list containing some of (u,v) AND some of (u',v') as input.
     *)
  let unroll ?(compact=false) ?(times=[]) ~from poss =
    let origs = List.map (fun (o,_) -> o) (triples_tls' poss) in
    call dec Dec.Zwrap.enable ();
    C.state_save "before_unroll";
    C.state_restore from;
    let ledger' = P.proxy dec (fun () -> P.data_get Dec.ledger) in
    C.state_restore "before_unroll";
    P.echo "Initial tradeline state";
    P.echo "═══════════════════════";
    P.echo (show_tradelines origs);

        (* Iterate on each reduction sequence *)
    let t_tls = triples_tls origs in
    Contractions.iter t_tls (fun contractions ->
        (*C.echo " BEFORE ITER\n ____________________\n" ;*)
        (*C.echo_env ();*)
        let players = List.filter (fun o -> orig_opt o = None) (owners_tls origs) in
        (* Iterate on each reduction *)
        (*P.echo_state ();*)
        P.echo "═════════════════════════════";
        let tl_with_contractions = Base.List.map t_tls
            ~f:(fun l ->
            Base.List.map l
              ~f:(fun (u,s,v) ->
                  let oi = Base.List.findi contractions ~f:(fun _ (_,_,_,v') -> v=v') in
                  let (i,(d,_,_,_)) = Option.get @@ oi in
                  (u,s,v,i,d))) in
        P.echo_pp "Playing sequence:\n %s\n"
          (show_tradelines_with_contractions tl_with_contractions);
          List.iter (fun arg ->
          crawl' times arg;
          if not compact then begin
            P.echo_pp "After move %s\n%s" ([%show: dir*Address.t*Address.t*Address.t] arg)


            ("Current tradeline state\n"^
             "───────────────────────\n"^
            (show_tradelines origs))
          end
          ) contractions;
        (*C.echo "before collect\n----------------------";*)
        (*C.echo_state ();*)
        collect players;
        (*C.echo "after collect\n-----------------";*)
        (*C.echo_state ();*)
        P.proxy dec (fun () ->
            let ledgerk' = P.data "ledger'" in
            P.data_set ledgerk' ledger';
            Ledger.Offchain.restrict Dec.ledger (fun o _m -> List.mem o players);
            Ledger.Offchain.update Dec.ledger
              (fun (o,index,t) a -> a - Ledger.balance ledgerk' o ~index t)
          );


        P.echo "Payoffs";
        P.echo "────────────────────";
        P.proxy dec (fun () ->
            Ledger.pp_custom Format.std_formatter (P.data_get Dec.ledger)
              (fun fmt addr index tk amount ->
                 if amount = 0 then () else
                 let prefix = if amount >= 0 then "+" else "" in
                 let index_str = if index = "" then "" else ("."^index) in
                 F.p fmt "%a%s: %s%d%a" Address.pp addr index_str prefix amount Address.pp tk;
                 F.cr ();
              ));
        P.echo "";
        C.state_restore "before_unroll";
      )
end


(** Compute all possible payoffs. Takes a starting position [pos]. Also a dec address [d]. The time for a specific pull.commit on a specific segment can be set with the optional [~times] (a list of (segment,direction,time) triples). [~from] specifies the name of the saved state to which payoffs should be compared (see [Chain.state_save]). *)
let payoffs ?(compact=false) ?(times=[]) ~from d pos =
  let module U = Make(struct let d = d end) in
  U.unroll ~compact ~times ~from [pos]
