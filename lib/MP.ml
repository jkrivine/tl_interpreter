module M = Base.Map.Poly
type ('k,'d) t = ('k,'d) M.t
let empty = M.empty
let update m k fn = M.update m k ~f:fn
let find_exn = M.find_exn
let find = M.find
let set m k d = M.set m ~key:k ~data:d
let remove = M.remove
let singleton = M.singleton
