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

(** A ledger reference that holds every user's token balances *)
val ledger   : Ledger.t data_id

(** Ownership map for positions and boxes. 
    positions are just addresses, but there is no corresponding contract on-chain (contract creation is expensive). In a real implementation, they could by any type. Boxes are also addresses. They represent the right ledger of a position. See [doc/dec_tutorial_1.png]. A position may be owned by any address (incl. another position or a box).

    An address is a position if it has a successor in [nexts] and it has an even number of predecessors in [nexts]. So after beeing freed by [free_singleton], a position is no longer a position.

    An address is a box if it has an odd number of predecessors in [nexts].

    None of those descriptions can be checked in constant time. The method [is_pos] is constant time. The operations of dec must be such that the execution of [is_pos] is semantically equivalent to checking whether the argument is a position.
*)
val owners   : (pos,pos) MP.t data_id

(** For every pos/box, pointer to the positon at the beginning of the tradeline. Should not be necessary in onchain operations but we maintain it here because it's comfortable. *)
val origins : (pos,pos) MP.t data_id

type parties = A.t * A.t

(** Legal operations are those that require to be called from a segment. The first argument is always a [(source,target)] pair representing the positions on each side of the calling segment. Note that multiple pairs can elect the same segment as their contract. This avoids needless contract creation. *)
module Legal : sig

  (** Extend a tradeline. The string argument is the display name of the new position. Since [grow] is part of [Legal], contracts control their future tradeline growth. *)
  val grow             : (parties * A.t * string * A.t, pos)          code_id

  (** Backward reduce. See the tutorial for more details on what happens during a pull. Briefly: the source position is kept. There are no ownership changes. The next segment (if any) is removed, and its target becomes the current segment's target. *)
  val pull             : (parties, unit)                              code_id

  (** Forward reduce. See the tutorial for more details on what happens during a commit. Briefly: the source position is kept and given to the target owner. The target box becomes the source's box. The source's box is given to the source's owner. The current segment is removed, and the target of the current segment becomes the target of the previous segment (if any). *)
  val commit           : (parties, unit)                              code_id

  (** Send tokens from left or right ledger to address. *)
  val transfer_token   : (parties * side * token * amount * A.t,unit) code_id

  (** Edit [owners] map. *)
  val transfer_address : (parties * side * A.t * A.t,unit)            code_id
end

(** Zwrapping is initiated by giving call pointers to [call_zwrap] *)
module Zwrap : sig
  val get_proxy : (unit,A.t)  code_id
  val enable    : (unit,unit) code_id
  val disable   : (unit,unit) code_id
  val test      : (unit,bool) code_id

  module Proxy : sig
    val construct : A.t -> unit
    module Magic : sig
      (** First address is the proxy address.
          Then [(caller,key,args)] :
         caller is the current caller, to be passed along (so proxy should be trusted)
         key is the code identifier to execute, which take sa caller as first arg and some 'a as second arg.
         args is is 'a args
      *)
      val call_zwrap : A.t -> (A.t * ((A.t*'a),'b) code_id * 'a) -> 'b

    end
  end
end

(** The following should be called from e.g. user-issued transactions. All functions raise if the caller does not have the authorization to execute the functions with the provided arguments. *)
module User : sig
  (** [new_pos "u"] creates a new position named "u" owned by caller. *)
  val new_pos           : (string, A.t)                      code_id

  (** Initial grow. A lone position can grow any segment. Subsequent grows
      should be done with [Legal.grow]. 

      Raises if not a singleton.
  *)
  val grow_singleton    : (A.t * A.t * string, A.t)          code_id

  (** A position or box is collectable when it is detached from any tradeline.
      As a special case, a singleton (pos,box) pair should be freed using
      [free_singleton] before it becomes collectable. *)

  (** [free_singleton u] is a cleanup method. It gives [u]'s box to [u]'s owner
      if [u] is a singleton. 
  
      Raises if not a singleton. *)
  val free_singleton    : (A.t,unit)                         code_id

  (** If [u] is collectable, give all of the tokens to [u]'s owner.
 
      Raises if not collectable. *)
  val collect_token     : (A.t * token, unit)                code_id

  (** If [a]'s owner is collectable, then [collect_address a] gives [a] to
      [a]'s owner's owner. 
      
      Raises if not collectable. *)
  val collect_address   : (A.t, unit)                        code_id

  (** [transfer_token t a ad] transfers [a] [t] from caller to [ad]. *)
  val transfer_token    : (token * amount * A.t, unit)       code_id

  (** [transfer_address u a] transfers [u] from caller to [a].  *)
  val transfer_address  : (A.t * A.t, unit)                  code_id

  (** Return owner of argument, raises if argument has no owner. *)
  val owner_of          : (A.t, A.t)                         code_id

  (** Option version of [owner_of] *)
  val owner_of_opt      : (A.t, A.t option)                  code_id

  (** Optionally returns the box of a pos. 
      
      Raises if argument is not a pos. *)
  val box_of            : (A.t, A.t option)                  code_id

  (** [balance_of a t] Gives Dec balance of [a] in token [t]. *)
  val balance_of        : (A.t * token, amount)              code_id

  (** [box_balance_of a t] Gives Dec balance of box of [a] in token [t]. 

      Raises under same conditions are [box_of] or [balance_of]. *)
  val box_balance_of    : (A.t * token, amount)              code_id

  (** [fund_with_token t a p side] gives [a t] to [p] or [p]'s box (depending
      on [side]), from the pockets of caller. *)
  val fund_with_token   : (token * amount * pos * side,unit) code_id

  (** [fund_with_token addr p side] gives [addr] to [p] or [p]'s box (depending
      on [side]), from the pockets of caller. *)
  val fund_with_address : (A.t * pos * side,unit)            code_id

  (** Not to be confused with the [nexts] map, [next_of u] for a position [u]
      returns the next position. Raises if [u] is not a position, returns None
      is there is no next position. *)
  val next_of           : (A.t, A.t option)                  code_id

  (** If [u] is a pos, [segment_of u] optionally returns the segment to the
      left of [u]. If [u] is a box, [segment_of u] optionally returns the
      segment to the right of [u]. *)
  val segment_of        : (A.t,A.t option)                   code_id

  (** Test if the argument is a pos *)
  val is_pos            : (A.t,bool)                         code_id

  (** Test if the argument is a pos originating a tradeline *)
  val is_origin         : (A.t,bool)                         code_id

  (** Test if the argument is a pos ending a tradeline *)
  val is_end            : (A.t,bool)                         code_id


  (** Test if the argument is both origin and end *)
  val is_singleton      : (A.t,bool)                         code_id

  (** {1 master_of} *)

  (** 

      Since a position or a box [u] is not a contract, one cannot directly authorize [u] to make calls. Checking [caller = owner_of p] is not sufficient because:
      - a position/box may be owned by a position/box
      - a box may not have an owner

      Given an owner [o] of [u], we want a contract currently considered to be "acting for" [o].
      - If [o] is not a pos/box, [master_of o = o]
      - If [o] has a segment, [master_of o = segment_of o]
      - Otherwise, [master_of o = master_of (owner_of o)] (this is not gas-bounded)
      - The case (no segment and no owner) should be impossible.

     It now suffices to check [caller = master_of (owner_of p)].
  *)
  val master_of         : (A.t, A.t)                         code_id
end


(** A simple exchange for trading positions/boxes against tokens *)
module Exchange : sig

  (** [make_ask address token amount] offers to sell [address] against [amount] [token]s. *)
  val make_ask : (A.t * A.t * amount,unit) code_id 

  (** [take_ask address token amount] accepts to buy [address] for [amount]
      [token]s. There must currently be an offer equal or better on the market.
      The owner must not have changed since the offer was introduced. *)
  val take_ask : (A.t * A.t * amount,unit) code_id

end



(** Dec constructor. Call this with [create_contract] to make a new Dec instance on the chain. *)
val construct : unit -> unit

(** Logging *)
val echo_dec : unit -> unit
