let sl = String.length

let me n = String.make n ' '

let rec mes s n = if n>0 then s^(mes s (n-1)) else ""

let ups s = ((sl s)-1)/2

let pre s = String.sub s 0 (ups s)

let post s = String.sub s ((ups s)+1) ((sl s)-(ups s)-1)

let mid s = String.sub s (ups s) 1

let ws_pre s = String.sub s 0 ((sl s)/2)
let ws_post s = String.sub s ((sl s)/2) ((sl s)-(sl s)/2)

(*
    bli blou
  8    4   10
  _  blo  ble
   *)

(* (bli,8),(blou,4),([],10) *)
let rec spread_inter lengths strings =
  let rec recurse acc prev = function
    | (str::xs,length::ys) ->
      let whitespace = me (length-(sl prev)-(sl (pre str))) in
      let content = (pre str)^(mid str) in
      recurse (acc^prev^whitespace^content) (post str) (xs,ys)
    | ([],length::xs) ->
      let whitespace = me (length-(sl prev)) in
      acc^prev^whitespace
    | (_,_) ->
      failwith "lengths list should be longer than strings list"
  in recurse "" "" (strings,lengths)

(* lengths size should be strings size + 2 *)
let spread_flush lengths strings =
  let rec recurse acc = function
        | (str::xs,length::ys) ->
          let l = length - (sl str) in
          let ws_pre, ws_post = me (l/2), me (l - l/2) in
          recurse (acc^ws_pre^str^ws_post^" ") (xs,ys)
        | ([],length::xs) -> acc^(me length)
        | (_,_) -> failwith "lengths list should be longer than strings list" in
  recurse "" ((""::strings),lengths)

let rec lines lengths =
  let rec recurse acc prev_line prev_stop = function
    | length::xs ->
      let line,stop = (match xs with
            _::_::_::_ -> "─","┼"
          | _::[_] -> "─","┤"
          | _ -> " ","") in
      recurse (acc^(mes prev_line length)^prev_stop) line stop xs
    | [] -> acc in
  recurse "" " " "├" lengths

let rec lengths_inter strings =
  let rec recurse prev = function
    | str::xs -> (sl prev)+(sl (pre str))::(recurse (post str) xs)
    | [] -> [sl prev] in
  recurse "" strings

let rec lengths_flush strings =
  let rec recurse = function
    | str::xs -> (sl str)::(recurse xs)
    | [] -> [0] in
  0::(recurse strings)

let rec diagonal lists =
  let rec recurse cur rest = function
    | (x::xs)::ls -> recurse (x::cur) (xs::rest) ls
    | [] -> cur :: (diagonal rest)
    | []::ls -> []
  in recurse [] [] lists

let max_lengths lengths =
  let ff = fun l -> List.fold_left max 0 l in
  let diag = diagonal lengths in
  List.map ff diag

let rec s_ls = function
  | x::xs -> (string_of_int x)^", "^(s_ls xs)
  | _ -> ""

let rec ss_ls = function
  | x::xs -> x^", "^(ss_ls xs)
  | _ -> ""

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


    (*
bliA         B         C
 u         v         w
*├─────────┼─────────┤*
_  loan(1)   loan(2)   _


       *)
