module Make () : sig

  (* Initialize blockchain state *)

  module P : module type of Env.Imp.Program
  module C : module type of Env.Imp.Chain

  type pos = Address.t
  type user = Address.t
  type contract = Address.t
  type token = Address.t
  type box = Address.t

  val box : pos -> box

  val init : user -> string -> pos

  val prev : pos -> pos

  val grow : pos -> contract -> string -> pos

  val time : unit -> int
  val time_set : int -> unit

  val first : unit -> bool

  val last : unit -> bool

  type asset = Token of Dec.token * Dec.amount | Position of Dec.pos
  val (~$) : int -> token -> asset
  val (~@) : pos -> asset

  val give : asset -> Address.t -> unit

  val pay : ?from:Address.t -> asset -> Address.t -> unit

  val transfer : Address.t -> asset -> Address.t -> unit

  val send : Address.t -> int -> token -> Address.t -> unit

  (* address can be token or pos *)
  val provision : Address.t -> Address.t -> int

  val reduce : unit -> unit

  val segment : string -> pull:(pos->pos->unit) -> commit:(pos->pos->unit) -> contract

val swap : Address.t -> asset -> Address.t -> asset -> unit

val set_payoffs_baseline : unit -> unit

val payoffs : compact:bool -> pos -> unit

val token : string -> token
val user : string -> user
val google : token
val euro : token
val alice : user
val bob : user
val carol : user
end


