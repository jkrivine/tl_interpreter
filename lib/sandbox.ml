module Make () = struct

  (* Initialize blockchain state *)

  module P = Env.Imp.Program
  module C = Env.Imp.Chain

  type currently = Pulling | Committing | InvalidCurrently
  type pos = Address.t
  type user = Address.t
  type contract = Address.t
  type token = Address.t
  type box = Address.t


  let currently = ref (InvalidCurrently,Address.admin,Address.admin) (*arbitrary*)

  let dec   = P.create_contract "dec"    Dec.construct   ()

  let box p = Option.get @@ P.call dec Dec.User.box_of p

  let init who pos_name =
    C.txr who dec Dec.User.new_pos pos_name

  let prev pos =
    let source = P.proxy dec (fun () -> P.map_find_exn Dec.origins pos) in
    let rec k p =
      let p' = Option.get @@ P.call dec Dec.User.next_of p in
      if p' = pos then p else k p'
    in k source

  let grow pos segment new_pos_name =
    let owner = P.call dec Dec.User.owner_of pos in
    if P.call dec Dec.User.is_end pos then begin
    if P.call dec Dec.User.is_singleton pos then
      C.txr owner dec Dec.User.grow_singleton (pos,segment,new_pos_name)
    else
      let segment' = Option.get @@ P.call dec Dec.User.segment_of pos in
      C.txr segment' dec Dec.Legal.grow ((prev pos,pos),segment,new_pos_name,owner)
  end else
      P.error "cannot grow non-end position"

  (*let after n =*)
    (*save_split_for_unroll (direction,n)*)
    (*P.time_get () > n*)

  (*let aftereq n =*)
    (*P.time_get () >= n*)

  (*let before n =*)
    (*P.time_get () < n*)

  (*let beforeeq n =*)
    (*P.time_get () <= n*)

  (*same for "last"*)
(*then :*)
  (*for each split point, for each combination,*)
                            (** compare state before/after*)
                            (** explore tree for every different state*)


  let time () = P.time_get ()
  let time_set n = C.time_set n

  let first () = match !currently with
    | (InvalidCurrently, _,_) -> P.error "Cannot call last outside of pull/commit"
    | (_,source,_target) ->
      P.call dec Dec.User.is_origin source

  let last () = match !currently with
    | (InvalidCurrently, _,_) -> P.error "Cannot call last outside of pull/commit"
    | (_,_source,target) ->
      P.call dec Dec.User.is_end target



  type asset = Token of Dec.token * Dec.amount | Position of Dec.pos
  let (~$) amount token = Token (token,amount)
  let (~@) pos = Position pos

  let give asset taker =
    match asset with
    | Position p ->
      let o = P.call dec Dec.User.owner_of p in
      C.tx o dec Dec.User.transfer_address (p,taker)
    | Token (token,amount) ->
      P.call token Token.mint_for (amount,taker)


  let rec pay ?from asset taker =
    match !currently with
    | (InvalidCurrently,_,_) ->
      P.error "Cannot call pay outside of pull/commit"
    | (_,source,target) ->
      match from with
      | None ->
        if taker = source then
          pay ~from:target asset (box source)
        else if taker = target then
          pay ~from:source asset target
        else
          P.error "on token payment, if ~from is unspecified, taker must be source or target"
      | Some f ->
        let giver =
          if f = target then Dec.Target else if f = source then Dec.Source
          else P.error "~from must be source or target" in
        match asset with
    | Position p ->
      P.call dec Dec.Legal.transfer_address ((source,target),giver,p,taker)
    | Token (token,amount) ->
        P.call dec Dec.Legal.transfer_token ((source,target),giver,token,amount,taker)

  let transfer giver asset taker =
    match asset with
    | Position p ->
      C.tx giver dec Dec.User.transfer_address (p,taker)
    | Token (token,amount) ->
      C.tx giver dec Dec.User.transfer_token (token,amount,taker)

  let send giver amount token taker =
      C.txr giver token Token.transfer (amount,taker)

  let provision who address =
    match !currently with
    | (InvalidCurrently,_,_) ->
      P.error "cannot call provision outside of pull/commit"
    | (_,source,target) ->
      let container = if who = source then box source else if who = target then target else P.error "who are you asking the provisions of?" in
      let as_token = P.call dec Dec.User.balance_of (container,address)
      in if as_token = 0
      then (if P.call dec Dec.User.owner_of address = container then 1 else 0)
      else as_token

  let reduce () =
    match !currently with
    | (InvalidCurrently,_,_) ->
      P.error "Cannot call reduce outside of pull/commit"
    | (Pulling,source,target) ->
      P.call dec Dec.Legal.pull (source,target)
    | (Committing,source,target) ->
      P.call dec Dec.Legal.commit (source,target)

  let segment name ~pull ~commit =

    let construct () =
      Segment.construct dec;

      P.code_set Segment.pull (fun (source,target) ->
          currently := (Pulling,source,target) ;
          pull source target ;
          currently := (InvalidCurrently,Address.admin,Address.admin));

      P.code_set Segment.commit (fun (source,target) ->
          currently := (Committing, source,target) ;
          commit source target ;
          currently := (InvalidCurrently,Address.admin,Address.admin))

    in P.create_contract name construct ()

  let swap user1 asset1 user2 asset2 =
    transfer user1 asset1 user2 ;
    transfer user2 asset2 user1

  let set_payoffs_baseline () = C.state_save "payoffs_baseline"

  let payoffs ~compact pos = Unroll.payoffs ~compact ~from:"payoffs_baseline" dec pos


  (* User transactions begin here *)

  let token name = P.create_contract name Token.construct ()
  let user name = P.create_user name

  let google = token "google"
  let euro = token "€"

  let alice = user "alice"
  let bob = user "bob"
  let carol = user "carol"

  let () =
    P.call dec Dec.Zwrap.enable ();
    give (~$ 1500 euro) alice ;
    give (~$ 40 google) alice ;

    give (~$ 1800 euro) bob ;
    give (~$ 60 google) bob ;

    give (~$ 2100 euro) carol ;
    give (~$ 80 google) carol ;

    send alice 200 euro  dec ;
    send alice 10 google dec ;

    send bob   400 euro  dec ;
    send bob   20 google dec ;

    send carol 600 euro  dec ;
    send carol 30 google dec ;


    set_payoffs_baseline () ;

    time_set 0
end

