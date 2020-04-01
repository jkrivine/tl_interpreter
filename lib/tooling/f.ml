(* formatting with custom indent because I don't like (or don't understand) how Format's indentation works *)

let indent = ref 0

let with_indent f =
  incr indent;
  let r = f () in
  decr indent;
  r

let cr () =
  if !indent < 0 then failwith "indents are not well-parenthesised" else begin
    let prefix = String.make (!indent * 2) ' ' in
    Format.fprintf Format.std_formatter "\n%s" prefix;
  end

let p fmt s =
  Format.fprintf fmt s
  (*Format.formatter ->*)
(*(unit, Format.formatter, unit, unit, unit, unit)*)
(*CamlinternalFormatBasics.format6 -> unit*)
(* shortcuts*)
let pp_string = Format.pp_print_string
let pp_int = Format.pp_print_int
let pp_bool = Format.pp_print_bool
let pp_char = Format.pp_print_char
let pp_float = Format.pp_print_float
