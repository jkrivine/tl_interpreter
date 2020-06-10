(** Displays diagrams such as *)
(** {v
bliA         B         C
 u         v         w
*├─────────┼─────────┤*
_  loan(1)   loan(2)   _

v}*)

let sl = String.length

(** [rp s n] repeats [s], [n] times*)
let rec rp s n = if n>0 then s^(rp s (n-1)) else ""

(** [ups s = ⌊(|s|-1)/2⌋] *)
let ups s = ((sl s)-1)/2

(** [pre s] is part of s before split *)
let pre s  = String.sub s 0 (ups s)

(** [post s] is part of s after split *)
let post s = String.sub s ((ups s)+1) ((sl s)-(ups s)-1)

(** [mid s] is part of s on split *)
let mid s  = String.sub s (ups s) 1

(**
   Some diagrams

    bli blou
  8    4   10
  _  blo  ble


bliA         B         C
 u         v         w
*├─────────┼─────────┤*
_  loan(1)   loan(2)   _

*)

(** [spread_inter lengths strings] spread [strings] between blocks of lengths
    [lengths].

    note that [|lenghts| = |strings|+1].

    Call stack example: [(bli,8),(blou,4),([],10)] *)
let spread_inter lengths strings =
  let rec recurse acc prev = function
    | (str::xs,length::ys) ->
      let whitespace = rp " " (length-(sl prev)-(sl (pre str))) in
      let content = (pre str)^(mid str) in
      recurse (acc^prev^whitespace^content) (post str) (xs,ys)
    | ([],length::_) ->
      let whitespace = rp " " (length-(sl prev)) in
      acc^prev^whitespace
    | (_,_) ->
      failwith "lengths list should be longer than strings list"
  in recurse "" "" (strings,lengths)


(** [spread_flush lengths strings]  spread [strings] above blocks of length
    [lengths], except first and last.

    Note that [|lengths| = |strings| + 2]
*)
let spread_flush lengths strings =
  let rec recurse acc = function
        | (str::xs,length::ys) ->
          let l = length - (sl str) in
          let whitespace_pre, whitespace_post = rp " " (l/2), rp " " (l - l/2) in
          recurse (acc^whitespace_pre^str^whitespace_post^" ") (xs,ys)
        | ([],length::_) -> acc^(rp " " length)
        | (_,_) -> failwith "lengths list should be longer than strings list" in
  recurse "" ((""::strings),lengths)

(** Show truncated lines of length lengths, except first and last *)
let _make_lines open_ line_ break_ close_ lengths =
  let rec recurse acc prev_line prev_stop = function
    | length::xs ->
      let line,stop = (match xs with
            _::_::_::_ -> line_, break_
          | _::[_] -> line_, close_
          | _ -> " ","") in
      recurse (acc^(rp prev_line length)^prev_stop) line stop xs
    | [] -> acc in
  recurse "" " " open_ lengths

(** Give best block lengths for strings that want to be between blocks *)
let lengths_inter strings =
  let rec recurse prev = function
    | str::xs -> (sl prev)+(sl (pre str))::(recurse (post str) xs)
    | [] -> [sl prev] in
  recurse "" strings

(** Give best block lengths for strings that want to be on top of blocks *)
let lengths_flush strings =
  let rec recurse = function
    | str::xs -> (sl str)::(recurse xs)
    | [] -> [0] in
  0::(recurse strings)

(** [max_lengths [l1;...;ln] = [max [l1(1);...;ln(1)];...; max [l1(n);...;ln(n)]]] *)
let max_lengths lengths =
  let ff = fun l -> List.fold_left max 0 l in
  let diag = Base.List.transpose_exn lengths in
  List.map ff diag

(** {1 Basic printing} *)

(** comma separate ints *)
let rec s_ls = function
  | x::xs -> (string_of_int x)^", "^(s_ls xs)
  | _ -> ""

(** comma separate strings *)
let rec ss_ls = function
  | x::xs -> x^", "^(ss_ls xs)
  | _ -> ""

type line_type = Default | LeftArrow | RightArrow
type line_content = Lines of line_type | Inter of string list | Flush of string list

let make_lines arg len = match arg with
  | Default -> _make_lines "├" "─" "┼" "┤" len
  | LeftArrow -> _make_lines "◀" "─" "┼" "┤" len
  | RightArrow -> _make_lines "├" "─" "┼" "▶" len


(** from_strings a b c returns, stacked : a (inter), b (inter), lines, c (flush) *)
let manual strings =
  let open List in
  let pad = map (fun s -> " "^s^" ") in
  let strings =
    strings
    |> map (function
        | Lines d -> Lines d
        | Flush s -> Flush (pad s)
        | Inter s -> Inter (pad s)) in
  let lengths =
    strings
    |> filter_map (function
        | Lines _ -> None
        | Flush s -> Some (lengths_flush s)
        | Inter s -> Some (lengths_inter s))
    |> max_lengths in

  strings
  |> map (function
      | Lines arg -> make_lines arg lengths
      | Inter s -> spread_inter lengths s
      | Flush s -> spread_flush lengths s)
  |> map (function s -> s^"\n")
  |> String.concat ""

let from_strings users positions segments =
  manual [Inter users; Inter positions; Lines Default; Flush segments]

let backward u seg v =
  manual [Inter [u;v]; Flush ["PULL"]; Lines LeftArrow; Flush [seg]]

let forward u seg v =
  manual [Inter [u;v]; Flush ["COMMIT"]; Lines RightArrow; Flush [seg]]

let test () =
  let inters = ["u";"v";"wakanda"] in
  let segments = ["loan";"superloan"] in
  let lengths = max_lengths [lengths_inter inters;lengths_flush segments] in
  print_endline (s_ls lengths);
  print_endline ("*"^(spread_inter lengths inters)^"*");
  print_endline ("*"^(spread_flush lengths segments)^"*");
  print_endline ("*"^(make_lines Default lengths)^"*")
