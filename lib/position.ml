module MM = Map.Make(struct type t = int let compare x y = compare x y end)

type pos = {
  id: int;
  fwd: conditions;
  bwd: provisions
}

and

time = int
and
time_conditions = MM
and
conditions = time_conditions Map.Make(struct
                                      type t = pos
                                      let compare x y = compare x.id y.id end).t
and
provisions = bool


