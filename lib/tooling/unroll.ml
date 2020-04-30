(* Warning! The code below is super ugly. You probably want to wait until it
   has been cleaned it up before reading it. *)

open Imperative
type dir = Pull | Commit
[@@deriving show { with_path = false }]

module Make(D:sig val d : Address.t end) = struct

  module Contractions = struct

    type t = (dir*int) list
    [@@deriving show]


    let iter length callback =
      let rec cts length index ct callback k =
        if (length < 0 || index < 0)
        then failwith "requires length >= 0 and index >= 0"
        else if length = 0
        then (callback ct ; k ())
        else if index >= length
        then k ()
        else
          let hedge () = cts length     (index+1) ct                callback k
          and fwd      = cts (length-1) 0         ((Commit,index)::ct) callback
          and bwd      = cts (length-1) 0         ((Pull,index)::ct) callback  in
          bwd (fun () -> fwd hedge)

      in  cts length 0 [] (fun l -> callback (List.rev l)) (fun () -> ())

    let map f length =
      let r = ref [] in
      iter length (fun l -> r := l::!r);
      List.map f (List.rev !r)

    let to_list length = map (fun l -> l) length


    let test () =
      iter 2 (fun l -> print_endline (show l))
  end

  let rec triples pos =
    let rec t' pos = match P.call D.d Dec.User.next_of pos with
      | None -> []
      | Some pos' -> (pos,Option.get (P.call D.d Dec.User.segment_of pos'),pos')::(t' pos')
    in t' pos

  let trail pos =
    let rec r x = function [] -> [x] | (p,_,p')::xs -> p::(r p' xs) in
    r pos (triples pos)

  let owners pos = List.map (fun pos -> P.call D.d Dec.User.owner_of pos) (trail pos)

  let segments pos = List.map (fun (_,s,_) -> s) (triples pos)

  let show_tradeline pos =
    let to_s l = List.map Address.to_string l in
    Lines.from_strings (to_s (owners pos)) (to_s (trail pos)) (to_s (segments pos))

  let crawl times pos (dir,index) = begin
    let ident_of = function Commit -> Segment.commit | Pull -> Segment.pull in
    let (p,s,p') = List.nth (triples pos) index in
    begin (* apply times *)
      (*P.echo ([%show: (Address.t * Address.t * Address.t * dir * int) list] times);*)
      (*P.echo (Address.to_string s);*)
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
    P.call s (ident_of dir) (p,p')
  end

  (* Does not handle a player owning another player *)
  let rec up_owner_chain owned ulti_owner f =
    if owned = ulti_owner then () else (
      ignore (f owned);
      (match P.call D.d Dec.User.owner_of_opt owned with
       | Some o' ->  up_owner_chain o' ulti_owner f
       | None -> ())
    )

  let rec first_in players owned =
    if List.mem owned players then Some owned else
      match P.call D.d Dec.User.owner_of_opt owned with
      | Some o' -> first_in players o'
      | None -> None

  let collect players =
    P.proxy D.d (fun () ->
        MP.iteri (P.data_get Dec.owners) @@ fun owned _ ->
        match first_in players owned with
        | None -> ()
        | Some first_owner ->
          up_owner_chain owned first_owner @@ fun owned ->
          Ledger.Offchain.iter_address_i Dec.ledger owned @@ fun token _ ->
          P.callthis Dec.User.collect_token (owned,token)
      )

  let rec next_nth p i =
    if i <= 0
    then p
    else next_nth (Option.get (P.call D.d Dec.User.next_of p)) (i-1)

  let unroll pos ?(times=[]) ~from =
    P.call D.d Dec.Zwrap.enable ();
    let index_adjust = function Commit -> 1 | Pull -> 0 in
    C.state_save "before_unroll";
    C.state_restore from;
    let ledger' = P.proxy D.d (fun () -> P.data_get Dec.ledger) in
    C.state_restore "before_unroll";
    P.echo "Initial tradeline state";
    P.echo "═══════════════════════";
    P.echo (show_tradeline pos);

    (* Iterate on each reduction sequence *)
    Contractions.iter (List.length (triples pos)) (fun contractions ->
        let players = List.sort_uniq compare @@
          List.map (fun p -> P.call D.d Dec.User.owner_of p) (trail pos) in
        let moves = ref [] in
        (* Iterate on each reduction *)
        P.echo_pp "Playing sequence: %s\n" (Contractions.show contractions);
        P.echo "═════════════════════════════";
        List.iter (fun (dir,index) ->
            moves := (dir, next_nth pos (index+(index_adjust dir)))::!moves;
            crawl times pos (dir,index);
            P.echo ("After move "^([%show: dir*Address.t] (List.hd !moves))^"\n"^
            "Current tradeline state\n"
            ^"───────────────────────\n"
            ^(show_tradeline pos))
          ) contractions;
        let owner = P.call D.d Dec.User.owner_of pos in
        C.tx owner D.d Dec.User.free_singleton pos;
        collect players;
        P.proxy D.d (fun () ->
            let ledgerk' = P.data "ledger'" in
            P.data_set ledgerk' ledger';
            Ledger.Offchain.restrict Dec.ledger (fun o m -> List.mem o players);
            Ledger.Offchain.update Dec.ledger
              (fun (o,index,t) a -> a - Ledger.balance ledgerk' o ~index t)
          );


        P.echo "Payoffs";
        P.echo "───────";
        P.proxy D.d (fun () ->
            Ledger.pp_custom Format.std_formatter (P.data_get Dec.ledger)
              (fun fmt addr index tk amount ->
                 if amount = 0 then () else
                 let prefix = if amount >= 0 then "+" else "" in
                 let index_str = if index = "" then "" else ("."^index) in
                 F.p fmt "%a%s: %s%d%a" Address.pp addr index_str prefix amount Dec.pp_token tk;
                 F.cr ();
              ));
        P.echo "";
        C.state_restore "before_unroll";
      )
end


let payoffs d pos ?(times=[]) ~from =
  let module U = Make(struct let d = d end) in
  U.unroll pos ~times ~from
