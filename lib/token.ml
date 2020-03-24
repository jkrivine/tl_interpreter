open Env

let balances = data ~pp:(MP.pp Address.pp Format.pp_print_int) ~init:MP.empty "balances"
let balance = code ()
let transfer = code ()
let transfer_up_to = code ()
let transfer_all = code ()

(* Also an external callback for others *)
let on_token_receive : (Address.t*Address.t*int,unit) code_hkey = code ()

module Admin = struct
  let mint_for token amount taker =
    proxy token (map_update balances taker ~default:0 (fun v -> v+amount))
end

let construct =

  let balance' who = map_find balances who |?* 0 in

  let add amount who =
    map_update balances who ~default:0 (fun v -> v+amount) in

  let transfer' giver amount taker =
    if amount < 0
    then error "cannot transfer a negative amount"
    else
      let* bal = balance' giver in
      if bal < amount
      then error "insufficient funds for transfer"
      else add (-amount) giver >> add amount taker  in

  let* token_addr = get_this in

  code_set on_token_receive (fun _ ->
      error "This contract does not receive tokens") >>


  code_set balance balance' >>

  code_set transfer (fun (amount,taker) ->
      let* giver = get_caller in
      transfer' giver amount taker >>
      if_responds taker on_token_receive (giver,token_addr,amount)    ) >>

  code_set transfer_up_to (fun (amount,taker) ->
      let* giver = get_caller in
      let* bal = balance' giver in
      transfer' giver (min amount bal) taker) >>

  code_set transfer_all (fun (taker) ->
      let* giver = get_caller in
      let* bal = balance' giver in
      transfer' giver bal taker)
