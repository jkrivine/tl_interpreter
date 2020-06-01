open Sandbox.Make ()

let () =

  let lender, trader = user "lender", user "trader" in

  let margin_reqs = oracle [(0,10);(5,15);(10,12)] in

  let long = segment "long / option"
      ~pull:(fun source target ->
          let req = consult margin_reqs in
          if provision target euro < req
          then (pay ~upto:true (~$ req euro) source ; reduce ())
        )
      ~commit:(fun source _ ->
          if time () > 7 then (pay (~$ 100 euro) source ; reduce ())
        ) () in


  let u = init lender "u" in

  let v = grow u long "v" in

  swap lender (~@ v) trader (~$ 1 euro) ;

  transfer lender (~$ 1 google) u ;

  fund_left (~$ 13 euro) v ;

  payoffs ~compact:true u ;






