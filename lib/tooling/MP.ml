(** Glue above [Base.Map.Poly] that removes argument names. *)

module M = Base.Map.Poly
type ('k,'d) t = ('k,'d) M.t
(*let pp fmt_k fmt_v fmt m =*)
  (*Format.fprintf fmt "{MP| ";*)
  (*M.iteri m ~f:(fun ~key ~data -> Format.fprintf fmt "%a: %a, " fmt_k key fmt_v data);*)
  (*Format.fprintf fmt "}"*)
let empty = M.empty
let of_alist_exn l =  M.of_alist_exn l
let update m k f = M.update m k ~f
let find_exn = M.find_exn
let find = M.find
let set m key data = M.set m ~key ~data
let remove = M.remove
let singleton = M.singleton
let takeout m k = (M.remove m k, M.find m k)
let change m k f = M.change m k ~f
let mem m k = M.mem m k
(*let find m k = M.find m k*)
let filter m f = M.filteri m ~f:(fun ~key ~data -> f key data)
let map m f = M.mapi m ~f:(fun ~key ~data -> f key data)
(* Print each map entry *)
let pp fmt_k fmt_d fmt m =
  if M.is_empty m then
    F.p fmt "<empty MP>"
  else begin
    (*F.with_indent (fun () ->*)
    M.iteri m ~f:(fun ~key ~data ->
        F.cr ();
        F.p fmt "%a: " fmt_k key;(* ᐅ *)
        F.with_indent (fun () -> F.p fmt "%a" fmt_d data);
        )
  end

let iteri m f = M.iteri m ~f:(fun ~key ~data -> f key data)

(* Custom printing of each line *)
let pp_i fmt iterator m =
  if M.is_empty m then
    F.p fmt "<empty MP>"
  else
    M.iteri m ~f:(fun ~key ~data -> iterator fmt key data)
