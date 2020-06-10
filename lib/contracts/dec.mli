(** {e Contract}. Implements core of tradelines *)

open Env.Imp.Program
(** Syntactic sugar *)
module MP = MP
module SP = SP
module A = Address

type token = A.t
[@@deriving show]

type amount = int
[@@deriving show]

type pos = A.t

val pp_pos : Format.formatter -> pos -> unit

(** Generic representation for choosing which side of a tradeline segment we're talking about. *)
type side = Source | Target
[@@deriving show]

val ledger   : Ledger.t data_id
val owners   : (pos,pos) MP.t data_id
val origins : (pos,pos) MP.t data_id

type parties = A.t*A.t

module Legal : sig
  (* Extend a tl *)
  val grow             : (parties * A.t * string * A.t, pos)          code_id
  (* Backward reduce *)
  val pull             : (parties, unit)                              code_id
  (* Forward reduce *)
  val commit           : (parties, unit)                              code_id
  (* Send tokens from left or right ledger to address *)
  val transfer_token   : (parties * side * token * amount * A.t,unit) code_id
  val transfer_address : (parties * side * A.t * A.t,unit)            code_id
end

module Zwrap : sig
  val get_proxy : (unit,A.t)  code_id
  val enable    : (unit,unit) code_id
  val disable   : (unit,unit) code_id
  val test      : (unit,bool) code_id

  module Proxy : sig
    val construct : A.t -> unit
    module Magic : sig
      (* First address is the proxy address.
         Then (caller,key,args) :
         caller is the current caller, to be passed along (so proxy should be trusted)
         key is the code identifier to execute, which take sa caller as first arg and some 'a as second arg.
         args is is 'a args
      *)
      val call_zwrap : A.t -> (A.t * ((A.t*'a),'b) code_id * 'a) -> 'b

    end
  end
end

module User : sig
  val new_pos           : (string, A.t)                      code_id
  val free_singleton    : (A.t,unit)                         code_id
  val grow_singleton    : (A.t * A.t * string, A.t)          code_id
  val collect_token     : (A.t * token, unit)                code_id
  val collect_address   : (A.t, unit)                        code_id
  val transfer_token    : (token * amount * A.t, unit)       code_id
  val transfer_address  : (A.t * A.t, unit)                  code_id
  val owner_of          : (A.t, A.t)                         code_id
  val owner_of_opt      : (A.t, A.t option)                  code_id
  val master_of         : (A.t, A.t)                         code_id
  val box_of            : (A.t, A.t option)                  code_id
  val balance_of        : (A.t * token, amount)              code_id
  val box_balance_of    : (A.t * token, amount)              code_id
  val fund_with_token   : (token * amount * pos * side,unit) code_id
  val fund_with_address : (A.t * pos * side,unit)            code_id
  val next_of           : (A.t, A.t option)                  code_id
  val segment_of        : (A.t,A.t option)                   code_id
  val is_pos            : (A.t,bool)                         code_id
  val is_origin         : (A.t,bool)                         code_id
  val is_end            : (A.t,bool)                         code_id
  val is_singleton      : (A.t,bool)                         code_id
end


module Exchange : sig

  val make_ask : (A.t * A.t * amount,unit) code_id 
  val take_ask : (A.t * A.t * amount,unit) code_id

end



val echo_dec : unit -> unit
val construct : unit -> unit
