open Imperative

module Loan = struct

  let repay = P.code ()
  let seize = P.code ()

  let construct dec ~time ~price:(tk,a) () =
    let pull = begin
      fun ((_source,target) as parties) ->
        let target_owner = P.call dec Dec.User.owner_of target in
        P.call dec Dec.Legal.transfer_token (parties,Dec.Source,tk,a,target_owner) ;
        P.call dec Dec.Legal.pull parties
    end in
    P.code_set repay pull;
    P.code_set Segment.pull pull;

    let commit = begin
      fun parties ->
        let current_time = P.time_get () in
        if current_time > time then
          P.call dec Dec.Legal.commit parties
        else
          P.error "too soon to seize collateral"
    end in
    P.code_set seize commit;
    P.code_set Segment.commit commit
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

    P.call google Token.mint_for (110,alice) ;
    P.call dollar Token.mint_for (3000,bob) ;
    P.call dollar Token.mint_for (500,alice) ;

    (* User transactions begin here *)

    let loan1 =
      C.tx_create alice "loan1" (Loan.construct dec ~time:10 ~price:(dollar,20)) () in

    let u = C.tx_with_return alice dec Dec.User.new_pos "u" in

    let v = C.tx_with_return alice dec Dec.User.grow_singleton (u,loan1,"v") in

    (* alice injects 100 google and 5 dollars in its Dec account *)
    C.tx alice google Token.transfer (100,dec) ;
    C.tx alice dollar Token.transfer (25,dec) ;

    (* bob injects 200 dollars in its Dec account *)
    C.tx bob dollar Token.transfer (200,dec) ;

    C.state_save "pre-loan" ;
    (* alice gives the 100 google to position u *)
    C.tx alice dec Dec.User.transfer_token (google,100,u) ;

    (* Trade pos for dollar *)
    C.tx alice dec Dec.Exchange.make_ask (v,dollar,18) ;
    C.tx bob dec Dec.Exchange.take_ask (v,dollar,18) ;
    C.echo "Loan was established" ;

    C.echo_env () ;
    C.state_save "loan" ;
    Unroll.payoffs dec u ~times:[(loan1,Unroll.Commit,11)] ~from:"pre-loan";
    ignore (fun () ->
    (* -- loan is setup, now exploring 2 possible scenarios -- *)

    (* -- alice pays back the loan -- *)
    C.time_incr 8 ;
    C.tx alice dec Dec.User.fund_with_token (dollar,20,u,Dec.Source) ;
    C.echo_env ();
    C.tx alice loan1 Loan.repay (u,v) ;
    C.echo "Loan was repaid" ;
    C.echo_env () ;

    (* -- bob grabs the collateral -- *)
    C.state_restore "loan" ;
    C.echo "Restoring initial state..." ;
    C.time_incr 11 ;
    C.tx bob loan1 Loan.seize (u,v) ;
    C.tx bob dec Dec.User.collect_token (u,google) ;
    C.echo "Loan was called in" ;
    C.echo_env ()
 ) (* ) *)

