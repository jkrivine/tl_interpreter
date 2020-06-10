(** Formatting with custom indent because I don't like (or don't understand) how [!Format] indentation works *)

(** Current indent level *)
let indent = ref 0

(** Run argument within a larger indent level. *)
let with_indent f =
  incr indent;
  let r = f () in
  decr indent;
  r

(** Newline + 2x as many spaces as current value of [!indent] *)
let cr () =
  if !indent < 0 then failwith "indents are not well-parenthesised" else begin
    let prefix = String.make (!indent * 2) ' ' in
    Format.fprintf Format.std_formatter "\n%s" prefix;
  end

(** Shortcut for Format.fprintf *)
let p fmt s =
  Format.fprintf fmt s


(** Display string *)
let s s =
  p Format.std_formatter "%s" s

(** Shortcut for printf *)
let pfn s =
  Format.kfprintf (fun _ -> ()) Format.std_formatter s

(** Shortcut for sprintf *)
let fs = Format.sprintf

(** {1 Common pretty-printing functions} *)
let pp_string = Format.pp_print_string
let pp_int = Format.pp_print_int
let pp_bool = Format.pp_print_bool
let pp_char = Format.pp_print_char
let pp_float = Format.pp_print_float

(** Pretty-print pairs *)
let pp_2 ppa ppb fmt (a,b) =
  let f = Format.pp_print_string fmt in
  f "("; ppa fmt a; f ", "; ppb fmt b; f ")"

(** Pretty-print 3-uples *)
let pp_3 ppa ppb ppc fmt (a,b,c) =
  let f = Format.pp_print_string fmt in
  f "("; ppa fmt a; f ", "; ppb fmt b; f ", "; ppc fmt c; f ")"
