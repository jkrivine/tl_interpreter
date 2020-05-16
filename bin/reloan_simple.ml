open Sandbox.Make ()

let () =

  let loan1 = segment
      "loan1"
      ~pull:(fun source _ ->
          if last () then pay (~$ 20 euro) source ;
          reduce ()
        )
      ~commit:(fun _ _ ->
          if time () > 10 then reduce ()
        ) in

  let u = init alice "u" in

  let v = grow u loan1 "v" in

  (*Imperative.P.echo_env ();*)

  transfer alice (~$ 100 google) u ;
  swap alice (~@ v) bob (~$ 18 euro) ;

  let loan2 = segment "loan2"
      ~pull:(fun source _ ->
          if last () then pay (~$ 18 euro) source;
          reduce ()
        )
      ~commit:(fun _ _ ->
          if time () > 8 then reduce ()
        ) in

  let w = grow v loan2 "w" in

  swap bob (~@ w) carol (~$ 16 euro) ;

  payoffs ~compact:true u






