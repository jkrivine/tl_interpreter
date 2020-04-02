open Env
(** Syntactic sugar *)
module MP = MP
module SP = SP
module A = Address

type token = A.t
[@@deriving show]

type amount = int

type pos
val pp_pos : Format.formatter -> pos -> unit

(** Generic representation for choosing which side of a tradeline segment we're talking about. *)
type side = Left | Right
[@@deriving show]

module Ledger : sig
  type t

  val balance : t data_hkey -> A.t -> ?index:string -> token -> amount st
  val transfer : t data_hkey -> A.t -> ?index:string -> token -> amount -> A.t -> unit st
  val transfer_up_to : t data_hkey -> A.t -> ?index:string -> token -> amount -> A.t -> unit st
  val transfer_all : t data_hkey -> A.t -> ?index:string -> token -> A.t -> unit st
  val solvent : t data_hkey -> bool st
  val pp : Format.formatter -> t -> unit
  val empty : t
end

val ledger    : Ledger.t data_hkey
val owners    : (A.t,A.t) MP.t data_hkey
val sources   : pos SP.t data_hkey
val nexts     : (pos,pos) MP.t data_hkey
val segments  : (pos,Address.t) MP.t data_hkey
val deads     : pos SP.t data_hkey 

val echo_dec : unit st

type parties = A.t*A.t

(* Change tl topology *)
val init_tl : (string * string * A.t, A.t * A.t) code_hkey
val grow : (parties * string * A.t * A.t, pos) code_hkey
val pull : (parties, unit) code_hkey
val commit  : (parties, unit) code_hkey
(* Transfers *)
val collect_token : (A.t * token, unit) code_hkey
val collect_address : (A.t * A.t, unit) code_hkey
val collect_box : (A.t, unit) code_hkey
val transfer_token : (token * amount * A.t, unit) code_hkey
val transfer_address : (A.t * A.t, unit) code_hkey
(* UNSAFE *)
val pay : (parties * side * token * amount * A.t,unit) code_hkey
(* read info *)
(* owners of boxes&positions are anything *)
val owner_of  : (A.t, A.t) code_hkey
(* a position may or may not have a box *)
(* pos -> prov *)
val box_of : (A.t, A.t option) code_hkey
(* any -> ... *)
val balance_of : (A.t * token, amount) code_hkey
(* Convenience composition of right_prov and get_balance *)
val box_balance_of : (A.t * token, amount) code_hkey

val is_zwrapping : (unit,bool) code_hkey
val get_zwrap_proxy : (unit,A.t) code_hkey
val construct : unit st

(*module Proxy : sig*)
  (*val bounce : (A.t * ((A.t,unit) code_hkey), unit) code_hkey*)
  (*val construct : A.t -> unit st*)
(*end*)

module ZwrapProxy : sig
  val construct : A.t -> unit st
  module Magic : sig
    val call_zwrap : A.t -> (A.t * ((A.t*'a),'b) code_hkey * 'a) -> 'b st
  end
end
