module Make () : sig

  (* Type synonyms *)
  type pos = Address.t
  type user = Address.t
  type contract = Address.t
  type token = Address.t
  type box = Address.t

  (* Resources *)
  val token : string -> token
  val user : ?defaults:bool -> string -> user

  val users : string list -> user list
  val google : token
  val euro : token
  val alice : user
  val bob : user
  val carol : user

  (* Moving funds *)
  type asset = Token of Dec.token * Dec.amount | Position of Dec.pos
  val (~$) : int -> token -> asset
  val (~@) : pos -> asset

  val give : int -> token -> Address.t -> unit
  val transfer : Address.t -> asset -> Address.t -> unit
  val send : Address.t -> int -> token -> Address.t -> unit
  val swap : Address.t -> asset -> Address.t -> asset -> unit
  val fund_left : ?from:Address.t -> asset -> pos -> unit
  val fund_right : ?from:Address.t -> asset -> pos -> unit

  (* External tradeline manipulation *)
  val init : user -> string -> pos
  val grow : pos -> contract -> string -> pos
  val segment : string -> ?on_connect:(pos->pos -> unit) -> ?pull:(pos->pos->unit) -> ?commit:(pos->pos->unit) -> unit -> contract
  
  (* Utility *)
  val box : pos -> box
  val prev : pos -> pos

  (* Functions used inside segments *)
  val first : unit -> bool
  val last : unit -> bool
  val pay : ?from:Address.t -> ?upto:bool -> asset -> Address.t -> unit
  val provision : Address.t -> Address.t -> int
  val reduce : unit -> unit

  (* Payoff calculation *)
  val set_payoffs_baseline : unit -> unit
  val payoffs : compact:bool -> pos -> unit

  (* Oracles *)
  val oracle : (int*int) list -> Address.t
  val consult : Address.t -> int

  (* Tradeline cannels *)
  type 'a chan
  val forward_channel : ?accumulate:('a -> 'a -> 'a) -> 'a -> 'a chan
  val backward_channel : ?accumulate:('a -> 'a -> 'a) -> 'a -> 'a chan
  val read : 'a chan -> 'a
  val write : 'a chan -> 'a -> unit
  val shift : 'a chan -> unit
  
  (* Access to low-level functions *)
  module P : module type of Env.Imp.Program
  module C : module type of Env.Imp.Chain

  (* State manipulation *)
  val time : unit -> int
  val time_set : int -> unit
  
end
