let sl = String.length

let rec rp s n = if n>0 then s^(rp s (n-1)) else "" (* repeat s, n times*)

let ups s = ((sl s)-1)/2 (* ⌊(|s|-1)/2⌋ *)

let pre s  = String.sub s 0 (ups s) (* part of s before split *)
let post s = String.sub s ((ups s)+1) ((sl s)-(ups s)-1) (* part of s after split *)
let mid s  = String.sub s (ups s) 1 (* part of s on split *)

(*
   Some diagrams

    bli blou
  8    4   10
  _  blo  ble


bliA         B         C
 u         v         w
*├─────────┼─────────┤*
_  loan(1)   loan(2)   _

*)

(* Spread strings between blocks of lengths lengths *)
(* |lenghts| = |strings|+1 *)
(* call stack example: (bli,8),(blou,4),([],10) *)
let rec spread_inter lengths strings =
  let rec recurse acc prev = function
    | (str::xs,length::ys) ->
      let whitespace = rp " " (length-(sl prev)-(sl (pre str))) in
      let content = (pre str)^(mid str) in
      recurse (acc^prev^whitespace^content) (post str) (xs,ys)
    | ([],length::xs) ->
      let whitespace = rp " " (length-(sl prev)) in
      acc^prev^whitespace
    | (_,_) ->
      failwith "lengths list should be longer than strings list"
  in recurse "" "" (strings,lengths)


(* Spread strings above blocks of length lengths, except first and last *)
(* |lengths| = |strings| + 2 *)
let spread_flush lengths strings =
  let rec recurse acc = function
        | (str::xs,length::ys) ->
          let l = length - (sl str) in
          let whitespace_pre, whitespace_post = rp " " (l/2), rp " " (l - l/2) in
          recurse (acc^whitespace_pre^str^whitespace_post^" ") (xs,ys)
        | ([],length::xs) -> acc^(rp " " length)
        | (_,_) -> failwith "lengths list should be longer than strings list" in
  recurse "" ((""::strings),lengths)

(* Show truncated lines of length lengths, except first and last *)
let rec lines lengths =
  let rec recurse acc prev_line prev_stop = function
    | length::xs ->
      let line,stop = (match xs with
            _::_::_::_ -> "─","┼"
          | _::[_] -> "─","┤"
          | _ -> " ","") in
      recurse (acc^(rp prev_line length)^prev_stop) line stop xs
    | [] -> acc in
  recurse "" " " "├" lengths

(* Give best block lengths for strings that want to be between blocks *)
let rec lengths_inter strings =
  let rec recurse prev = function
    | str::xs -> (sl prev)+(sl (pre str))::(recurse (post str) xs)
    | [] -> [sl prev] in
  recurse "" strings

(* Give best block lengths for strings that want to be on top of blocks *)
let rec lengths_flush strings =
  let rec recurse = function
    | str::xs -> (sl str)::(recurse xs)
    | [] -> [0] in
  0::(recurse strings)

(* Diagonal symmetry for list of lists *)
let rec diagonal lists =
  let rec recurse cur rest = function
    | (x::xs)::ls -> recurse (x::cur) (xs::rest) ls
    | [] -> cur :: (diagonal rest)
    | []::ls -> []
  in recurse [] [] lists

(* max_lengths [l1;...;ln] = [max [l1(1);...;ln(1)];...; max [l1(n);...;ln(n)]] *)
let max_lengths lengths =
  let ff = fun l -> List.fold_left max 0 l in
  let diag = diagonal lengths in
  List.map ff diag

(* Basic printing *)
let rec s_ls = function
  | x::xs -> (string_of_int x)^", "^(s_ls xs)
  | _ -> ""

let rec ss_ls = function
  | x::xs -> x^", "^(ss_ls xs)
  | _ -> ""

(* from_strings a b c returns, stacked : a (inter), b (inter), lines, c (flush) *)
let from_strings users positions segments =
  let lengths = max_lengths
      [lengths_inter users; lengths_inter positions; lengths_flush segments] in
  (spread_inter lengths users)^"\n"^
  (spread_inter lengths positions)^"\n"^
  (lines lengths)^"\n"^
  (spread_flush lengths segments)^"\n"

let test () =
  let inters = ["u";"v";"wakanda"] in
  let segments = ["loan";"superloan"] in
  let lengths = max_lengths [lengths_inter inters;lengths_flush segments] in
  print_endline (s_ls lengths);
  print_endline ("*"^(spread_inter lengths inters)^"*");
  print_endline ("*"^(spread_flush lengths segments)^"*");
  print_endline ("*"^(lines lengths)^"*")
