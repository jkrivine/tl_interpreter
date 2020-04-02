open Env

module Loan = struct
  let repay = code ()
  let seize = code ()
  let construct dec ~time ~price:(tk,a) =
    let* zwrapper = call dec Dec.get_zwrapper () in
    Macro.zwrap zwrapper repay (fun (caller,((seller,buyer) as parties)) ->
        let* seller_owner = call dec Dec.owner_of seller in
        (*call dec Dec.transfer_token_from_buyer (parties,buyer_owner,a,t) >>*)

        call dec Dec.pay (parties,Dec.Right,tk,a,seller_owner) >>
        call dec Dec.pull parties) >>
    Macro.zwrap zwrapper seize (fun (caller,parties) ->
        let* current_time = time_get in
        if current_time > time then
          call dec Dec.commit parties
        else
          return ())
end

let () = ignore ( execute (
    time_set 0 >>
    let* (dec,_) = create_contract "dec" Dec.construct in
    let* (google,_) = create_contract "google" Token.construct in
    let* (dollar,_) = create_contract "$" Token.construct in
    let* uA = create_user "user A" in
    let* uB = create_user "user B" in
    Token.Admin.mint_for google 110 uA >>
    Token.Admin.mint_for dollar 3000 uB >>
    Token.Admin.mint_for dollar 5 uA >>
    (* User transactions begin here *)
    let* (loan1,u,v) = proxy uA begin
        let* (loan1,_) = create_contract "loan1" (Loan.construct dec ~time:10 ~price:(dollar,20)) in
        let* (u,v) = call dec Dec.init_tl ("u","v",loan1) in
        return (loan1,u,v)
      end in
    (* uA injects 100 google and 5 dollars in its Dec account *)
    tx uA google Token.transfer (100,dec) >>
    tx uA dollar Token.transfer (5,dec) >>
    (* uB injects 200 dollars in its Dec account *)
    tx uB dollar Token.transfer (200,dec) >>
    (* uA gives the 100 google to position u *)
    tx uA dec Dec.transfer_token (google,100,u) >>
    (* Trade pos for dollar *)
    (* 1. uB gives $18 to uA *)
    tx uB dollar Token.transfer (18,uA) >>
    (* 2. uA gives v to uB *)
    tx uA dec Dec.transfer_address (v,uB) >>
    echo "Loan was established" >>
    echo_env >>
    state_save "loan" >>
    time_incr 8 >>
    (* uA pays back *)
    tx uA loan1 Loan.repay (u,v) >>
    echo "Loan was repaid" >>
      echo_env >>
    (* uB grabs the collateral *)
    state_restore "loan" >>
    echo "Restoring initial state..." >>
    time_incr 11 >>
    tx uB loan1 Loan.seize (u,v) >>
    tx uB dec Dec.collect_token (u,google) >>
    echo "Loan was called in" >>
    echo_env
  ))
