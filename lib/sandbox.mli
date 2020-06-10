(** Very simplified API for creating example contracts. 
    
    See [doc] for a visual tutorial. 
    
    See [bin/demos] for examples. 

    It's a functor so that doing [open Sandbox.Make ()] puts fresh values in scope.

*)
module Make () : sig

  (** {1 Type synonyms} *)

  (** A tradeline position *)
  type pos = Address.t

  (** A tradeline position's box (i.e. right ledger) *)
  type box = Address.t

  type user = Address.t
  type contract = Address.t
  type token = Address.t

  (** {1 Resources} *)

  (** {2 Functions} *)

  (** Create a new named token *)
  val token : string -> token

  (** Create a new named user, optionally with default amounts of [google] and [euro]. *)
  val user : ?defaults:bool -> string -> user

  (** {2 Default tokens and users} *)
  val google : token
  val euro : token
  val alice : user
  val bob : user
  val carol : user

  (** {1 Moving funds} *)

  (** Some amount of a fungible token, or a tradeline position *)
  type asset = Token of Dec.token * Dec.amount | Position of Dec.pos

  (** [~$ i t = Token (t,i)] *)
  val (~$) : int -> token -> asset

  (** [~@ u = Position u] *)
  val (~@) : pos -> asset


  (** {e (outside of Dec)} [give i t a] generates [i] amount of token [t] for address [a] *)
  val give : int -> token -> Address.t -> unit

  (** {e (outside of Dec)} [send a i t b] transfers [i] of token [t] from
      address [a] to address [b]. *)
  val send : Address.t -> int -> token -> Address.t -> unit

  (** {e (in Dec)} [transfer a asset b] moves [asset] from [a] to [b] within Dec's ledgers *)
  val transfer : Address.t -> asset -> Address.t -> unit

  (** {e (in Dec)} Convenience, atomic double transfer. *)
  val swap : Address.t -> asset -> Address.t -> asset -> unit

  (** {e (in Dec)} [fund_left asset pos] moves [asset] from [pos]'s owner (or
      [~from] if specified) to [pos]. *)
  val fund_left : ?from:Address.t -> asset -> pos -> unit

  (** {e (in Dec)} [fund_right asset pos] moves [asset] from [pos]'s owner (or
      [~from] if specified) to [pos]'s box. *)
  val fund_right : ?from:Address.t -> asset -> pos -> unit

  (** {1 External tradeline manipulation} *)

  (** [init u s] creates a fresh position named [s], owned by [u]. *)
  val init : user -> string -> pos

  (** [grow p c s] creates a fresh position with name [s] owned by [p]'s owner,
      to the right of [p], with segment [c] between them. *)
  val grow : pos -> contract -> string -> pos

  (** Create a new segment contract. [~on_connect] is triggered whenever the
      contract is used in a [grow]. [~pull] is used whenever the key
      [Segment.pull] is called on the contract's address. [~commit] is called
      whenever [Segment.commit] is called. The arguments of each callback are the current [source] and [target]. *)
  val segment : string -> ?on_connect:(pos->pos -> unit) -> ?pull:(pos->pos->unit) -> ?commit:(pos->pos->unit) -> unit -> contract

  (** {1 Use those inside segment callbacks} *)

  (** {2 Tests} *)

  (** True if first in line. *)
  val first : unit -> bool

  (** True if last in line. *)
  val last : unit -> bool

  (** {2 Balances} *)

  (** Simple use: do not specify [~from], only use the callback's first argument ([source]) or second argument ([target]) as the last argument of [pay]:
      - [pay asset source] will transfer [asset] from [target] to [source]'s box.
      - [pay asset target] will transfer [asset] from [source]'s box to [target].

      Advanced use:
      - [~upto] does not try to remove more than the current balance in the giver
      - [~from] specifies the giver of the asset. In that case, the last argument of [pay] will be understood literally.
  *)
  val pay : ?from:Address.t -> ?upto:bool -> asset -> Address.t -> unit

  (** To be used on a position or a box. [provision b euro] returns how many
      euros (within Dec) [b] has. If [u] is a positoin or a box, [provision b u]
      returns 1 if [b] owns [u], 0 otherwise. *)
  val provision : Address.t -> Address.t -> int

  (** {2 Reduction} *)

  (** Reduces by pulling if called in the [~pull] callback, by committing if
      called in the [~commit] callback. Raises if called in the [~on_connect]
      callback. *)
  val reduce : unit -> unit

  (** {2 Oracles} *)

  (** [oracle [(t0,v0);(t1,v1)]] creates a contract which responds to [consult]
      with value [v0] at times [t0 <= t < t1] and value [v1] at times [t1
      <= t] *)
  val oracle : (int*int) list -> Address.t
  val consult : Address.t -> int

  (** {2 Channels} *)

  type 'a chan
  val forward_channel : ?accumulate:('a -> 'a -> 'a) -> 'a -> 'a chan
  val backward_channel : ?accumulate:('a -> 'a -> 'a) -> 'a -> 'a chan
  val read : 'a chan -> 'a
  val write : 'a chan -> 'a -> unit
  val shift : 'a chan -> unit

  (* {1 Payoff calculation} *)

  (** Save current state as baseline for computing payoffs *)
  val set_payoffs_baseline : unit -> unit

  (** Run all possible pull/commit combinations, moving time if necessary.
      Display payoffs in each case. *)
  val payoffs : compact:bool -> pos -> unit

  (** {1 Misc. utility} *)

  (** Owner of position *)
  val owner : pos -> user

  (** Box (i.e. right ledger) of position *)
  val box : pos -> box

  (** Position to the left. Raises if first in line. *)
  val prev : pos -> pos

  (** Position to the right. Raises if last in line. *)
  val next : pos -> pos
  
  (* Access to low-level functions *)
  module P : module type of Env.Imp.Program
  module C : module type of Env.Imp.Chain

  (* State manipulation *)
  val time : unit -> int
  val time_set : int -> unit
  
end
