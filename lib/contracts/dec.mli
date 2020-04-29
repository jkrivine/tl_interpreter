open Imperative.P
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

val ledger   : Ledger.t data_identifier
val owners   : (pos,pos) MP.t data_identifier

type parties = A.t*A.t

module Legal : sig
  (* Extend a tl *)
  val grow             : (parties * string * A.t * A.t, pos) code_identifier
  (* Backward reduce *)
  val pull             : (parties, unit) code_identifier
  (* Forward reduce *)
  val commit           : (parties, unit) code_identifier
  (* Send tokens from left or right ledger to address *)
  val transfer_token   : (parties * side * token * amount * A.t,unit) code_identifier
  val transfer_address : (parties * side * A.t * A.t,unit) code_identifier
end

module Zwrap : sig
  val get_proxy : (unit,A.t)  code_identifier
  val enable    : (unit,unit) code_identifier
  val disable   : (unit,unit) code_identifier
  val test      : (unit,bool) code_identifier

  module Proxy : sig
    val construct : A.t -> unit
    module Magic : sig
      (* First address is the proxy address.
         Then (caller,key,args) :
         caller is the current caller, to be passed along (so proxy should be trusted)
         key is the code identifier to execute, which take sa caller as first arg and some 'a as second arg.
         args is is 'a args
      *)
      val call_zwrap : A.t -> (A.t * ((A.t*'a),'b) code_identifier * 'a) -> 'b

    end
  end
end

module User : sig
  (* Start a new tl with 2 positions *)
  val init_tl          : (string * string * A.t, A.t * A.t) code_identifier
  (* Transfers *)
  val collect_token    : (A.t * token, unit) code_identifier
  val collect_address  : (A.t, unit) code_identifier
  (*val collect_box      : (A.t, unit) code_identifier*)
  val transfer_token   : (token * amount * A.t, unit) code_identifier
  val transfer_address : (A.t * A.t, unit) code_identifier
  (* UNSAFE *)
  (*val pay              : (parties * side * token * amount * A.t,unit) code_identifier*)
  (* read info *)
  (* owners of boxes&positions are anything *)
  val owner_of         : (A.t, A.t) code_identifier
  val owner_of_opt     : (A.t, A.t option) code_identifier
  (* a position may or may not have a box *)
  (* pos -> prov *)
  val box_of           : (A.t, A.t option) code_identifier
  (* any -> ... *)
  val balance_of       : (A.t * token, amount) code_identifier
  (* Convenience composition of right_prov and get_balance *)
  val box_balance_of   : (A.t * token, amount) code_identifier
  val fund_with_token  : (token * amount * pos * side,unit) code_identifier
  val fund_with_address : (A.t * pos * side,unit) code_identifier
  val next_of :           (A.t, A.t option) code_identifier
  val segment_of        : (A.t,A.t option) code_identifier
end

val echo_dec : unit -> unit
val construct : unit -> unit
