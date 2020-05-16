open Base

let (|?) a default = Option.value a ~default
let pp_to_str pp a =
  ignore @@ Format.flush_str_formatter ();
  pp Format.str_formatter a;
  Format.flush_str_formatter ()

