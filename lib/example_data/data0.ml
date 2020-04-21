open Imperative

let () =
  C.initialize_chain ()

let alice = P.create_user "alice"
let bob = P.create_user "bob"
let dollar = P.create_contract "dollar" Token.construct ()

let euro = P.create_contract "euro" Token.construct ()

let () =
  P.call dollar Token.mint_for (11,alice);
  P.call euro Token.mint_for (9,alice);
  P.call euro Token.mint_for (9,bob)




