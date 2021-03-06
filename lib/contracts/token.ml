(** {e Contract}. Implements basic ERC20-like functionality. *)
open Tools
open Env.Imp.Program

(** A simple [address->int] map *)
let balances = data ~pp:(MP.pp Address.pp Format.pp_print_int) "balances"

(** Whoever has minting power on the token *)
let owner = data ~pp:Address.pp "owner"

let balance = code ()
let transfer = code ()
let transfer_up_to = code ()
let transfer_all = code ()

(** admin only *)
let mint_for = code ()

(** All transfers attempt to call [on_token_receive] on the receiving contract. Ignored if method is not implemented by the contract. *)
let on_token_receive : (Address.t*Address.t*int,unit) code_id = code ()

let construct () =

  let caller = get_caller () in
  data_set owner caller ;
  data_set balances MP.empty ;

  code_set mint_for begin
    fun (amount,taker) ->
      require (data_get owner = get_caller ());
      map_update balances taker ~default:0 (fun v -> v+amount)
  end;

  let balance' who = map_find balances who |? 0 in

  let add amount who =
    map_update balances who ~default:0 (fun v -> v+amount) in

  let transfer' giver amount taker =
    if amount < 0
    then error "cannot transfer a negative amount"
    else
      let bal = balance' giver in
      if bal < amount
      then error "insufficient funds for transfer"
      else
        add (-amount) giver; add amount taker in

  let token_addr = get_this () in

  code_set on_token_receive (fun _ ->
      error "This contract does not receive tokens");


  code_set balance balance' ;

  code_set transfer (fun (amount,taker) ->
      let giver = get_caller () in
      transfer' giver amount taker;
      if responds taker on_token_receive then
        call taker on_token_receive (giver,token_addr,amount)
      else ());

  code_set transfer_up_to (fun (amount,taker) ->
      let giver = get_caller () in
      let bal = balance' giver in
      transfer' giver (min amount bal) taker);

  code_set transfer_all (fun (taker) ->
      let giver = get_caller () in
      let bal = balance' giver in
      transfer' giver bal taker)
