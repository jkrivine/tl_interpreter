open Dec_types

(*module type State = sig*)
  (*type t*)
  (*val read : t -> int*)
  (*val write : t -> entry option -> int -> t (* None for God *)*)
(*end*)

(*module Oracle = struct*)
  (*type t = int*)
  (*let read o = o*)
  (*let write t -> = function None -> fun x -> x | Some _ -> failwith "Not allowed" (* Only god can write *)*)
(*end*)

(*module Switch = struct*)
  (*type t = {owner: entry; val: int}*)
  (*let read o = s.val*)
  (*let make e i = {owner = e; val = i}*)
  (*let write s = function*)
    (*| None | Some e when e = s.owner -> fun j -> make s.owner j*)
    (*| Some _ -> failwith "Not allowed" (* Only god and owner canwrite *)*)



type t = time -> amount

let read o t = o t

