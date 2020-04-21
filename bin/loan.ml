open Imperative

module Loan = struct

  let repay = P.code ()
  let seize = P.code ()

  let construct dec ~time ~price:(tk,a) () =

    P.code_set repay begin
      fun ((source,_target) as parties) ->
        let source_owner = P.call dec Dec.User.owner_of source in
        P.call dec Dec.Legal.transfer_token (parties,Dec.Target,tk,a,source_owner) ;
        P.call dec Dec.Legal.pull parties
    end ;

    P.code_set seize begin
      fun parties ->
        let current_time = P.time_get () in
        if current_time > time then
          P.call dec Dec.Legal.commit parties
        else
          P.return ()
    end
end

let () =

    C.initialize_chain ();

  (* Initialize blockchain state *)
    C.time_set 0 ;

    let dec    = P.create_contract "dec"    Dec.construct   () in
    let google = P.create_contract "google" Token.construct () in
    let dollar = P.create_contract "$"      Token.construct () in

    let uA = P.create_user "user A" in
    let uB = P.create_user "user B" in

    P.call google Token.mint_for (110,uA) ;
    P.call dollar Token.mint_for (3000,uB) ;
    P.call dollar Token.mint_for (5,uA) ;

    (* User transactions begin here *)

    let loan1 =
      C.tx_create uA "loan1" (Loan.construct dec ~time:10 ~price:(dollar,20)) () in

    let (u,v) = C.tx_with_return uA dec Dec.User.init_tl ("u","v",loan1) in


    (* uA injects 100 google and 5 dollars in its Dec account *)
    C.tx uA google Token.transfer (100,dec) ;
    C.tx uA dollar Token.transfer (5,dec) ;

    (* uB injects 200 dollars in its Dec account *)
    C.tx uB dollar Token.transfer (200,dec) ;

    (* uA gives the 100 google to position u *)
    C.tx uA dec Dec.User.transfer_token (google,100,u) ;

    (* Trade pos for dollar *)
    (* 1. uB gives $18 to uA *)
    C.tx uB dollar Token.transfer (18,uA) ;
    (* 2. uA gives v to uB *)
    C.tx uA dec Dec.User.transfer_address (v,uB) ;
    C.echo "Loan was established" ;

    C.echo_env () ;
    C.state_save "loan" ;
    (* -- loan is setup, now exploring 2 possible scenarios -- *)

    (* -- uA pays back the loan -- *)
    C.time_incr 8 ;
    C.tx uA dec Dec.User.fund_with_token (dollar,20,u,Dec.Source) ;
    C.tx uA loan1 Loan.repay (u,v) ;
    C.echo "Loan was repaid" ;
    C.echo_env () ;

    (* -- uB grabs the collateral -- *)
    C.state_restore "loan" ;
    C.echo "Restoring initial state..." ;
    C.time_incr 11 ;
    C.tx uB loan1 Loan.seize (u,v) ;
    C.tx uB dec Dec.User.collect_token (u,google) ;
    C.echo "Loan was called in" ;
    C.echo_env ()

