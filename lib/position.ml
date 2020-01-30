open Base

type user = int
type time = int
type amount = int

type pos = int

type segment = {
  sel_pos: pos;
  buy_pos: pos;
  fwd_contract: pos (clause list) Map.Poly.t;
  bwd_contract: clause list;
  boxes: (box * user) amount Map.Poly.t;
}

type line = {
  owners: pos user Map.Poly.t;
  next: pos (pos option) Map.Poly.t;
  prev: pos (pos option) Map.Poly.t;
}

type test = BoxHas of box*amount | BoxEmpty of box
type act = EmptyBox of box

type clause = {
  from: time option;
to: time option;
  test: test list;
  act: act list;
}

