module S = Core.Set.Poly
type 'a t = 'a S.t
let pp fmt_v fmt m =
  Format.fprintf fmt "{SP| ";
  S.iter m ~f:(fun v -> Format.fprintf fmt "%a, " fmt_v v);
  Format.fprintf fmt "}"
let empty = S.empty
let add = S.add
let mem = S.mem
