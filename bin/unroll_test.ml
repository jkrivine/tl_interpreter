type dir = Bwd | Fwd
[@@deriving show { with_path = false }]

type contractions = (dir*int) list
[@@deriving show]


let iter length callback =
  let rec cts length index ct callback k =
    if (length < 0 || index < 0)
    then failwith "requires length >= 0 and index >= 1"
    else if length = 0
    then (callback ct ; k ())
    else if index >= length
    then k ()
    else
      let hedge () = cts length     (index+1) ct                callback k
      and fwd      = cts (length-1) 0         ((Fwd,index)::ct) callback
      and bwd      = cts (length-1) 0         ((Bwd,index)::ct) callback  in
      bwd (fun () -> fwd hedge)

  in  cts length 0 [] (fun l -> callback (List.rev l)) (fun () -> ())

let map f length =
  let r = ref [] in
  iter length (fun l -> r := l::!r);
  List.map f (List.rev !r)

let to_list length = map (fun l -> l) length


let () =
  iter 2 (fun l -> print_endline (show_contractions l))


