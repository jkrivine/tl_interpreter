(** The Dec (DEcentralized Clearing) manages the global state of all tradelines and associated provisions *)

(** For convenience, [Dec_types] contains most of the Dec's non-functional type definitions *)
include module type of Dec_types

(** Contains the current state *)
type t

(** Calls are state-modifying instructions given by some caller. Most of them run authorization checks before executing.

  [REDUCE p q side clause] contracts the positions [p], [q] by triggering [clause]. [side] specifies whether the contraction is a forward or a backward move.

  [GROW p s] extends a tradeline ending with [p], with forward/backward contracts specified by [s]

  [PROVISION p t a] gives [a] of the caller's token [t] to position [p]

  [COLLECT p t] gives all tokens [t] owned by postion [p] to [p]'s owner

  [NEW] initializes a new tradeline

  [MAKE_CALL f] executes the call given by [f a t] where [a] is the caller and [t] is the current time. *)
type call = 
  | REDUCE of pos * pos * side * clause 
  | GROW of pos * segment 
  | PROVISION of pos * token * amount 
  | COLLECT of pos * token 
  | NEW
  | MAKE_CALL of (addr -> time -> call) 

(** [exec m t l] takes the current state [m] as a first argument and a list of [(caller,call)] pairs. It returns the state after execution of those [call]s. Throws if a call could not be executed, or if the resulting state would contain an insolvent Dec. *)
val exec : t -> time -> (addr * call) list -> t

(** [init_tl m a] is a public-facing execution of [NEW] *)
val init_tl : t -> addr -> t

type fwd_contract := (pos, clause list) MP.t

(** [call_grow_A] is an example of a call-building function. It builds a restricted form of backward contracts given by some duration parameters. Since those durations depend on the when the call is executed, [call_grow_A] returns a [MAKE_CALL f] where the application of [f] returns a [GROW] call. *)
val call_grow_A :
  pos -> fwd_contract -> time -> token -> amount -> time -> call

