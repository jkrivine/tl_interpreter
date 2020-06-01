(* Put the sandbox tools in scope and initialize a useful blockchain state. *)
open Sandbox.Make ()

let () =

  (* This loan contract is named "loan1".

     Anyone can pull unless they would become the end of the tradeline. In that
     case, there must be 20€ provisioned to pay. Note that when computing
     payoffs we alow zcrossing across transactions!

     Commit is always possible from time 10 onwards.

     Note that `reduce ()` means pull inside the ~pull function and means
     commit inside the `commit` function.

     Each of those functions takes the current source and targets as arguments.
  *)

  let loan1 = segment
      "loan1"
      ~pull:(fun _ target ->
          if last () then pay (~$ 20 euro) target ;
          reduce ()
        )
      ~commit:(fun _ _ ->
          if time () > 10 then reduce ()
        ) () in

  (* `init` starts a tradeline with a single position named `"u"`, owned by `alice` *)
  let u = init alice "u" in

  (* `grow` creates a position `"v"` next to `u`. The segment between them is
     `loan1`. By default `u`'s owner will own `v` *)
  let v = grow u loan1 "v" in

  (* `fund_left` sends 100 google stocks from `alice` to `u`. `fund_left` sends
     the asset to the lef-facing provision of `u`. `fund_right` would have sent
     it to its right-facing provision. *)
  fund_left (~$ 100 google) u ;

  (* `swap` trades assets between two parties ; here `alice` sends `v` to
     `bob`, who in turn sends 18€ to `alice`. *)
  swap alice (~@ v) bob (~$ 18 euro) ;

  (* This is another loan segment with a different name (`"loan2"`) and
     different parameters. *)
  let loan2 = segment "loan2"
      ~pull:(fun _ target ->
          if last () then pay (~$ 18 euro) target;
          reduce ()
        )
      ~commit:(fun _ _ ->
          if time () > 8 then reduce ()
        ) () in

  (* Again we grow the tradeline. This time the new position is named `"w"`. *)
  let w = grow v loan2 "w" in

  (* Another swap, this time between `bob` and `carol` *)
  swap bob (~@ w) carol (~$ 16 euro) ;

  (* Start payoff simulation & pretty-printing. ~compact:false to display each intermediat ereduction step *)
  payoffs ~compact:true u
