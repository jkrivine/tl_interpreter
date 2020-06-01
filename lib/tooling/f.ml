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

let s s =
  p Format.std_formatter "%s" s

let pfn s =
  Format.kfprintf (fun _ -> ()) Format.std_formatter s

let fs = Format.sprintf

let pp_string = Format.pp_print_string
let pp_int = Format.pp_print_int
let pp_bool = Format.pp_print_bool
let pp_char = Format.pp_print_char
let pp_float = Format.pp_print_float

let pp_2 ppa ppb fmt (a,b) =
  let f = Format.pp_print_string fmt in
  f "("; ppa fmt a; f ", "; ppb fmt b; f ")"

let pp_3 ppa ppb ppc fmt (a,b,c) =
  let f = Format.pp_print_string fmt in
  f "("; ppa fmt a; f ", "; ppb fmt b; f ", "; ppc fmt c; f ")"
