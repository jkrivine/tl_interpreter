(** A simple module for addresses *)

(** Addresses are mainly used as keys in the global chain state. *)

type t
[@@deriving show]

(** Generate a fresh address. 
    [next "jane"] will be displayed as [jane] in logging facilities.
*)
val next : string -> t

(** Special admin address.
    Only address authorized to e.g. initiate a proxy call *)
val admin : t
