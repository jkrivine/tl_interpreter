type t = int * string
let counter = ref 0
(*let pp fmt (_,s) = F.p fmt "⦃ %s ⦄" s*)
let pp fmt (_,s) = F.p fmt "%s" s
(*let pp fmt (_,s) = F.p fmt "⟨%s⟩" s*)
(*let pp fmt (_,s) = F.p fmt "⦑ %s ⦒" s*)
(*[@@deriving show]*)

let eq = (=)
let next s =
  let ret = (!counter + 1,s) in
  incr counter; ret

let admin = next "<admin>"
let show a =
  let open Format in
  ignore (flush_str_formatter ());
  pp str_formatter a;
  flush_str_formatter ()
