open Imperative
type dir = Bwd | Fwd
[@@deriving show { with_path = false }]

module Make(D:sig val d : Address.t end) = struct

  module Contractions = struct

    type contractions = (dir*int) list
    [@@deriving show]


    let iter length callback =
      let rec cts length index ct callback k =
        if (length < 0 || index < 0)
        then failwith "requires length >= 0 and index >= 1"
        else if length = 0
        then (callback ct ; k ())
        else if index >= length
        then k ()
        else
          let hedge () = cts length     (index+1) ct                callback k
          and fwd      = cts (length-1) 1         ((Fwd,index)::ct) callback
          and bwd      = cts (length-1) 1         ((Bwd,index)::ct) callback  in
          bwd (fun () -> fwd hedge)

      in  cts length 0 [] (fun l -> callback (List.rev l)) (fun () -> ())

    let map f length =
      let r = ref [] in
      iter length (fun l -> r := l::!r);
      List.map f (List.rev !r)

    let to_list length = map (fun l -> l) length


    let test () =
      iter 2 (fun l -> print_endline (show_contractions l))
  end

  let rec triples pos =
    let rec t' pos = match P.call D.d Dec.User.next_of pos with
      | None -> []
      | Some pos' -> (pos,Option.get (P.call D.d Dec.User.segment_of pos'),pos')::(t' pos')
    in t' pos

  let tail pos =
    let rec r x = function [] -> x | (p,_,p')::xs -> p::(r [p'] xs) in
    r [] (triples pos)


  type tb = (Dec.A.t * Dec.A.t * Dec.A.t) list
  [@@deriving show]

  let crawl times pos (dir,index) = begin
    let ident_of = function Fwd -> Segment.commit | Bwd -> Segment.pull in
    (*pp_tb Format.std_formatter (triples pos);*)
    (*F.pp_int Format.std_formatter index;*)
    let (p,s,p') = List.nth (triples pos) index in
    (
    (match List.find_opt (fun ((_,_,_,_,t) as tup) -> tup = (p,s,p',dir,t)) times with
    | Some (_,_,_,_,t) -> C.time_set t
    | None -> ());
    P.call s (ident_of dir) (p,p'))
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

  let unroll ?(times=[]) ~from pos =
    P.call D.d Dec.Zwrap.enable ();
    let index_adjust = function Fwd -> 1 | Bwd -> 0 in
    C.state_save "before_unroll";
    C.state_restore from;
    let ledger' = P.proxy D.d (fun () -> P.data_get Dec.ledger) in
    C.state_restore "before_unroll";
    Contractions.iter (List.length (triples pos)) (fun contractions ->
        let players = List.sort_uniq compare @@ List.map (fun p -> P.call D.d Dec.User.owner_of p) (tail pos) in
        F.pfn "Players: %s" ([%derive.show: Address.t list] players);
        let moves = ref [] in
        List.iter (fun (dir,index) ->
            moves := (dir, next_nth pos (index+(index_adjust dir)))::!moves;
            crawl times pos (dir,index)
          ) contractions;
        P.echo ([%derive.show: (dir*Address.t) list] !moves);
        collect players;
        P.proxy D.d (fun () ->
            let ledgerk' = P.data "ledger'" in
            P.data_set ledgerk' ledger';
            Ledger.Offchain.restrict Dec.ledger (fun o m -> List.mem o players);
            Ledger.Offchain.update Dec.ledger
              (fun (o,index,t) a -> a - Ledger.balance ledgerk' o ~index t)
          );


        P.proxy D.d (fun () ->
            Ledger.pp_custom Format.std_formatter (P.data_get Dec.ledger)
              (fun fmt addr index tk amount ->
                 if amount = 0 then () else
                 let prefix = if amount >= 0 then "+" else "" in
                 let index_str = if index = "" then "" else ("."^index) in
                 F.cr (); F.p fmt "%a%s: %s%d%a" Address.pp addr index_str prefix amount Dec.pp_token tk));
        P.echo "";
        C.state_restore "before_unroll";
      )
end
