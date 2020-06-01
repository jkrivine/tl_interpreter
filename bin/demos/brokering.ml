open Sandbox.Make ()

let () =

  let seller, broker, buyer = user "seller", user "broker", user "buyer" in

  let prices = forward_channel ~accumulate:(+) 0 in
  let brokering ~price ~deadline =
    segment (F.fs "brokering(%i,%i)" price deadline)
      ~on_connect:(fun _ _ ->
          write prices price
        )
      ~pull:(fun _ _ ->
          if time () > deadline then reduce ()
        )
      ~commit:(fun source _ ->
          let r = read prices in
          pay (~$ r euro) source ;
          shift prices;
          reduce ()
        ) () in

let u = init seller "seller_p"  in

  fund_left (~$ 1 google) u ;

  let v = grow u (brokering ~price:10 ~deadline:7) "broker_p" in

  swap seller (~@ v) broker (~$ 0 euro) ;

  let w = grow v (brokering ~price:1 ~deadline:5) "buyer_p" in

  swap broker (~@ w) buyer (~$ 0 euro) ;

  payoffs ~compact:true u ;






