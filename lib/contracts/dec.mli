open Env
(** Syntactic sugar *)
module MP = MP
module SP = SP
module A = Address

type token = A.t
[@@deriving show]

type amount = int

type pos = A.t

val pp_pos : Format.formatter -> pos -> unit

(** Generic representation for choosing which side of a tradeline segment we're talking about. *)
type side = Source | Target
[@@deriving show]

type parties = A.t*A.t

module Legal : sig
  (* Extend a tl *)
  val grow             : (parties * string * A.t * A.t, pos) code_hkey
  (* Backward reduce *)
  val pull             : (parties, unit) code_hkey
  (* Forward reduce *)
  val commit           : (parties, unit) code_hkey
  (* Send tokens from left or right ledger to address *)
  val transfer_token   : (parties * side * token * amount * A.t,unit) code_hkey
  val transfer_address : (parties * side * A.t * A.t,unit) code_hkey
end

module Zwrap : sig
  val get_proxy : (unit,A.t)  code_hkey
  val enable    : (unit,unit) code_hkey
  val disable   : (unit,unit) code_hkey
  val test      : (unit,bool) code_hkey

  module Proxy : sig
    val construct : A.t -> unit st
    module Magic : sig
      val call_zwrap : A.t -> (A.t * ((A.t*'a),'b) code_hkey * 'a) -> 'b st
    end
  end
end

module User : sig
  (* Start a new tl with 2 positions *)
  val init_tl          : (string * string * A.t, A.t * A.t) code_hkey
  (* Transfers *)
  val collect_token    : (A.t * token, unit) code_hkey
  val collect_address  : (A.t, unit) code_hkey
  val transfer_token   : (token * amount * A.t, unit) code_hkey
  val transfer_address : (A.t * A.t, unit) code_hkey
  (* UNSAFE *)
  (*val pay              : (parties * side * token * amount * A.t,unit) code_hkey*)
  (* read info *)
  (* owners of boxes&positions are anything *)
  val owner_of         : (A.t, A.t) code_hkey
  (* a position may or may not have a box *)
  (* pos -> prov *)
  val box_of           : (A.t, A.t option) code_hkey
  (* any -> ... *)
  val balance_of       : (A.t * token, amount) code_hkey
  (* Convenience composition of right_prov and get_balance *)
  val box_balance_of   : (A.t * token, amount) code_hkey
  val fund_with_token  : (token * amount * pos * side,unit) code_hkey
  val fund_with_address : (A.t * pos * side,unit) code_hkey
end

val echo_dec : unit st
val construct : unit st
