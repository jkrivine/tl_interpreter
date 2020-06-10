module P = Env.Imp.Program
module C = Env.Imp.Chain

module Loan = struct


  let construct dec ~time ~price:(tk,a) () =
    Segment.construct dec;

    (* repay *)
    P.code_set Segment.pull begin
      fun ((_source,target) as parties) ->
        (if P.call dec Dec.User.is_end target then
           let target_owner = P.call dec Dec.User.owner_of target in
           P.call dec Dec.Legal.transfer_token (parties,Dec.Source,tk,a,target_owner) ;
         else
           ());
        P.call dec Dec.Legal.pull parties
    end ;

    (* seize *)
    P.code_set Segment.commit begin
      fun parties ->
        let current_time = P.time_get () in
        if current_time > time then
          P.call dec Dec.Legal.commit parties
        else
          P.error "too soon to seize collateral"
    end

end

let () =

    C.initialize_chain ();

  (* Initialize blockchain state *)
    C.time_set 0 ;

    let dec    = P.create_contract "dec"    Dec.construct   () in
    let google = P.create_contract "google" Token.construct () in
    let dollar = P.create_contract "$"      Token.construct () in

    let alice = P.create_user "alice" in
    let bob = P.create_user "bob" in
    let carol = P.create_user "carol" in

    P.call google Token.mint_for (110,alice) ;
    P.call dollar Token.mint_for (3000,bob) ;
    P.call dollar Token.mint_for (500,alice) ;
    P.call dollar Token.mint_for (5000,carol) ;

    (* User transactions begin here *)

    (* Dec funding *)
    C.tx alice google Token.transfer (1,dec) ;
    C.tx alice dollar Token.transfer (100,dec) ;
    C.tx bob dollar Token.transfer (200,dec) ;
    C.tx carol dollar Token.transfer (1000,dec) ;

    (* Init tl *)

    let loan1 =
      C.tx_create alice "loan1" (Loan.construct dec ~time:10 ~price:(dollar,20)) () in

    let u = C.txr alice dec Dec.User.new_pos "u" in

    let v = C.txr alice dec Dec.User.grow_singleton (u,loan1,"v") in

    C.state_save "pre-loan" ;

    (* fund tl *)
    C.tx alice dec Dec.User.transfer_token (google,1,u) ;
    (* exchange 1 *)
    C.tx alice dec Dec.Exchange.make_ask (v,dollar,18) ;
    C.tx bob dec Dec.Exchange.take_ask (v,dollar,18) ;

    C.echo "Loan was established" ;

    let loan2 =
      C.tx_create bob "loan2" (Loan.construct dec ~time:8 ~price:(dollar,18)) () in

    let w = C.txr  bob loan1 Segment.grow ((u,v),loan2,"w") in

    C.tx bob dec Dec.Exchange.make_ask (w,dollar,16) ;
    C.tx carol dec Dec.Exchange.take_ask (w,dollar,16) ;

    C.echo_env () ;
    Unroll.payoffs ~times:[(loan1,Unroll.Commit,11);(loan2,Unroll.Commit,13)] ~from:"pre-loan" dec u;
