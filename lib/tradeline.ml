include Tradeline_types


let (|?) a default = Option.value a ~default

let ledger_balance ledger player =
  Option.value (MP.find ledger player) 0

let transfer tl pos player =
  {tl with owners = MP.set tl.owners pos player}

let default_segment = {
  fwd_contract = MP.empty;
  bwd_contract = [];
  ledger = MP.empty
}

let provision tl pos player amount =
  {tl with segments = MP.change tl.segments pos
  (fun s_opt ->
    let segment = s_opt |? default_segment in
    let update_amount a_opt = Some (amount + (a_opt |? 0)) in
    Some {segment with ledger = MP.change segment.ledger player update_amount}
  )}

let reduce tl pos side time clause = failwith "Not implemented"

let gc tl pos side time p_opt clause = failwith "Not implemented"

let init addr = failwith "Not implemented"

let grow tl segment = failwith "Not implemented"

let bind tl asset = failwith "Not implemented"

let unbind tl = failwith "Not implemented"
