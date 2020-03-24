open Env
(** Syntactic sugar *)
module MP = MP
module SP = SP

type token = Address.t
[@@deriving show]

type amount = int

type pos
val pp_pos : Format.formatter -> pos -> unit

(** Generic representation for choosing which side of a tradeline segment we're talking about. *)
type side = Seller | Buyer
[@@deriving show]

(** Ledger entries can represent either users or positions *)
and entry = Eaddr of Address.t | Epos of pos
(*val pp_entry : Format.formatter -> entry -> unit*)

module Ledger : sig
  type t

  val balance : t data_hkey -> entry -> token -> amount st
  val transfer : t data_hkey -> entry -> entry -> token -> amount -> unit st
  val transfer_up_to : t data_hkey -> entry -> entry -> token -> amount -> unit st
  val transfer_all : t data_hkey -> entry -> entry -> token -> unit st
  val solvent : t data_hkey -> bool st
  val pp : Format.formatter -> t -> unit
  val empty : t
end

val ledger    : Ledger.t data_hkey
val owners    : (pos,entry) MP.t data_hkey
val sources   : pos SP.t data_hkey
val nexts     : (pos,pos) MP.t data_hkey
val segments  : (pos,Address.t) MP.t data_hkey
val deads     : pos SP.t data_hkey 
val max_pos   : int data_hkey

val print_dec : unit st

type parties = pos*pos

(* Change tl topology *)
val init_tl : (string*string*Address.t,pos*pos) code_hkey
val grow : (parties * string * entry * Address.t, pos) code_hkey
val pull : (parties,unit) code_hkey
val commit  : (parties,unit) code_hkey
(* Transfers *)
val pay : (parties * side * entry * token * amount,unit) code_hkey
val draw_up_to : (parties * token * amount, unit) code_hkey
val collect_token : (pos * token,unit) code_hkey
val collect_pos : (pos * pos,unit) code_hkey
(* Reading *)
val owner_of : (pos,entry) code_hkey
val get_provision : (parties * token,amount) code_hkey
val provision_hte : (parties * entry * token * amount,bool) code_hkey
val provision_lt : (parties * entry * token * amount,bool) code_hkey

val transfer_token_to_provision : (token*amount*pos,unit) code_hkey
val transfer_pos_to_provision : (pos*pos,unit) code_hkey
val transfer_token : (token*amount*Address.t,unit) code_hkey
val transfer_pos : (pos*Address.t,unit) code_hkey

val construct : unit st
