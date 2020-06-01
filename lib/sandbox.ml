module Make () = struct

  (* Initialize blockchain state *)

  module P = Env.Imp.Program
  module C = Env.Imp.Chain

  type pos = Address.t
  type currently = Invalid | Connecting of pos*pos | Pulling of pos*pos | Committing of pos*pos
  type user = Address.t
  type contract = Address.t
  type token = Address.t
  type box = Address.t


  let currently = ref Invalid
  let if_reducing_dir operation ~pull ~commit = match !currently with
    | Pulling (s,t) -> pull s t
    | Committing (s,t) -> commit s t
    | _ -> P.error @@ "Cannot call "^operation^" outside of pull/commit"

  let if_reducing operation fn =
    if_reducing_dir operation ~pull:fn ~commit:fn

  (* only in a segment context *)
  let if_valid operation fn = match !currently with
    | Pulling (s,t) | Committing (s,t) | Connecting (s,t) -> fn s t
    | Invalid -> P.error @@ "Cannot call "^operation^" outside of segment"



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
    if P.call dec Dec.User.is_end pos then
      let new_pos =
        if P.call dec Dec.User.is_singleton pos then
          C.txr owner dec Dec.User.grow_singleton (pos,segment,new_pos_name)
        else
          let segment' = Option.get @@ P.call dec Dec.User.segment_of pos in
          C.txr segment' dec Dec.Legal.grow ((prev pos,pos),segment,new_pos_name,owner)
      in C.txr owner segment Segment.on_connect (pos,new_pos) ; new_pos
    else
      P.error "cannot grow non-end position"

  let time () = P.time_get ()
  let time_set n = C.time_set n

  let first () =
    if_valid "first" @@ fun source _ -> P.call dec Dec.User.is_origin source

  let last () =
    if_valid "last" @@ fun _ target -> P.call dec Dec.User.is_end target

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


  let rec pay ?from ?(upto=false) asset taker =
    if_valid "pay" @@ fun source target ->
    match from with
    | None ->
      if taker = source then
        pay ~from:target ~upto asset (box source)
      else if taker = target then
        pay ~from:source ~upto asset target
      else
        P.error "on token payment, if ~from is unspecified, taker must be source or target"
    | Some giver ->
      let side =
        if giver = target then Dec.Target else if giver = source then Dec.Source
        else P.error "~from must be source or target" in
      match asset with
      | Position p ->
        P.call dec Dec.Legal.transfer_address ((source,target),side,p,taker)
      | Token (token,amount) ->
        let amount' =
          if upto
          then min (P.call dec Dec.User.balance_of (giver,token)) amount
          else amount in
        P.call dec Dec.Legal.transfer_token ((source,target),side,token,amount',taker)

  let transfer giver asset taker =
    match asset with
    | Position p ->
      C.tx giver dec Dec.User.transfer_address (p,taker)
    | Token (token,amount) ->
      C.tx giver dec Dec.User.transfer_token (token,amount,taker)

  let send giver amount token taker =
      C.txr giver token Token.transfer (amount,taker)

  let provision who address =
    if_valid "provision" @@ fun source target ->
    let container = if who = source then box source else if who = target then target else P.error "who are you asking the provisions of?" in
    let as_token = P.call dec Dec.User.balance_of (container,address)
    in if as_token = 0
    then (if P.call dec Dec.User.owner_of address = container then 1 else 0)
    else as_token

  let rec fund_left ?from asset pos =
    match from with
    | None -> fund_left ~from:(P.call dec Dec.User.owner_of pos) asset pos
    | Some giver ->
      transfer giver asset pos

  let rec fund_right ?from asset pos =
    match from with
    | None -> fund_right ~from:(P.call dec Dec.User.owner_of pos) asset pos
    | Some giver ->
      transfer giver asset (box pos)

  let reduce () =
    if_reducing_dir "reduce"
      ~pull:(fun source target -> P.call dec Dec.Legal.pull (source,target))
      ~commit:(fun source target -> P.call dec Dec.Legal.commit (source,target))

  let ignore2 = fun _ _ -> ()
  let segment name ?(on_connect=ignore2) ?(pull=ignore2) ?(commit=ignore2) () =

    let construct () =
      Segment.construct dec;

      P.code_set Segment.pull (fun (source,target) ->
          currently := Pulling (source,target) ;
          pull source target ;
          currently := Invalid);

      P.code_set Segment.commit (fun (source,target) ->
          currently := Committing (source,target) ;
          commit source target ;
          currently := Invalid);

      P.code_set Segment.on_connect (fun (source,target) ->
          currently := Connecting (source,target) ;
          on_connect source target ;
          currently := Invalid)

    in
    P.create_contract name construct ()

  let swap user1 asset1 user2 asset2 =
    transfer user1 asset1 user2 ;
    transfer user2 asset2 user1

  let set_payoffs_baseline () = C.state_save "payoffs_baseline"

  let payoffs ~compact pos = Unroll.payoffs ~compact ~from:"payoffs_baseline" dec pos


  (* User transactions begin here *)

  let token name = P.create_contract name Token.construct ()

  let google = token "google"
  let euro = token "€"

  let oracles = ref MP.empty

  let oracle values =
    let s = MP.length !oracles in
    let o = P.create_empty_contract ("oracle "^(string_of_int s)) in
    oracles := MP.set !oracles o values ;
    o

  let consult oracle =
    let t = P.time_get () in
    let v = MP.find_exn !oracles oracle in
    let (_,r) = List.hd @@ List.rev @@ List.filter (fun (t',_) -> t >= t') v in r

  let user ?(defaults=true) name =
    let user = P.create_user name in
    if defaults then begin

      give (~$ 1500 euro) user ;
      give (~$ 30 google) user ;

      send user 200 euro  dec ;
      send user 10 google dec
    end ;
    set_payoffs_baseline () ;
    user

  let users l = List.map (fun u -> user ~defaults:true u) l

  let alice = user ~defaults:false "alice"
  let bob = user ~defaults:false "bob"
  let carol = user ~defaults:false "carol"

  (* I could represent all canals as simple addresses, but
     then read/write/shift would not be polymorphic *)
  type a = Address.t
  type 'a chan = {
    address: a;
    read: (a*a*a) -> 'a;
    write: (a*a*a*'a) -> unit;
    shift: (a*a*a) -> unit
  }

  let forward_channel_counter = ref 0
  let backward_channel_counter = ref 0

  let _channel ~forward ?(accumulate=(fun x _ -> x)) default =
    let counter = if forward then forward_channel_counter else backward_channel_counter in
    let c = !counter in
    let prefix = if forward then "forward" else "backward" in
    incr counter ;
    let vals = P.data_hidden () in
    let write = P.code () in
    let read = P.code () in
    let shift = P.code () in
    let chan =
      P.create_contract (prefix^" channel #"^(string_of_int c)) (fun _ ->
          P.data_set vals MP.empty ;

          P.code_set write (fun (target,value) ->
              let current = P.callthis read (P.get_caller (),target) in
              P.map_set vals (P.get_caller (),target) @@ accumulate value current
            ) ;

          P.code_set read (fun (segment,target) ->
              match P.map_find vals (segment,target) with
              | Some e -> e
              | None -> default);

          P.code_set shift (fun (source,target) ->
              let current = P.callthis read (P.get_caller (),target) in

              if forward then
                match P.call dec Dec.User.next_of target with
                | None -> ()
                | Some target' ->
                  (match P.call dec Dec.User.segment_of target' with
                   | None -> ()
                   | Some segment' ->
                     let current' = P.callthis read (segment',target') in
                     P.map_set vals (segment',target') @@ accumulate current current');
                  P.map_set vals (P.get_caller (),target) default

              else
                (match P.call dec Dec.User.segment_of source with
                | None -> ()
                | Some segment' ->
                  let current' = P.callthis read (segment',source) in
                  P.map_set vals (segment',source) @@ accumulate current current');
                  P.map_set vals (P.get_caller (),target) default

            )
        ) () in
    {address=chan;
     read= (fun (_source,seg,target) ->
         P.call chan read (seg,target));
     write = (fun (_source,_seg,target,value) ->
         P.call chan write (target,value));
     shift = (fun (source,_seg,target) ->
         P.call chan shift (source,target))
    }

  let forward_channel ?(accumulate=(fun x _ -> x)) default =
    _channel ~forward:true ~accumulate default

  let backward_channel ?(accumulate=(fun x _ -> x)) default =
    _channel ~forward:true ~accumulate default

  let read chan =
    if_valid "read" (fun source target ->
        let seg = Option.get @@ P.call dec Dec.User.segment_of target in
        chan.read (source,seg,target))

  let write chan value =
    if_valid "write" (fun source target ->
        let seg = Option.get @@ P.call dec Dec.User.segment_of target in
      chan.write (source,seg,target,value))

  let shift chan =
    if_valid "shift" (fun source target ->
        let seg = Option.get @@ P.call dec Dec.User.segment_of target in
        chan.shift (source,seg,target)
      )

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

