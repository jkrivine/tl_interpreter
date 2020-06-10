module Nucleus : sig
  (** [se] is for State+Exception monad *)
  type 'a se

(** The general theme here is: contracts are executed in a context specifying
   `this`, which is an address.  There is a global storage which associates a
   heterogenous map to each address.  This map can contain data, code, etc.

   Given a key [k: int key], which is a key to get a value of type int, the
    result of accessing [k] depends on the current value of [this]. Since the
   context is hidden in the pseudo-state monad, contracts can just say 
    [read k]. 
    2 instances of the same contract at 2 different addresses will return a
   different value.

   Those keys also map to code. So if there are two instances of a contract
   [C], one at address [ad1] and the other at address [ad2], and [C] exposes
   the key [k: (int -> int) key], running [call ad1 C.k] and [call ad2 C.k] may
   not run the same code.  *)
  type ('a,'b) code_id
  type 'a data_id
end

(** There are two implementations of the [Program] and [Chain] APIs. One is functional and lives in [Fun.*]. The other is imperative and lives in
    [Imp.*]. *)
module Fun : sig

  (** The [st] type is for chain commands. In the functional version, chain commands are of the monadic type [Nucleus.se].
      The [unit_st] type is for commands that don't need an argument to be run -- they only need a context.
      [unit_st] in the functional case is a synonym for [st]. 
  *)
  module Program : C.Program
    with type 'a st = 'a Nucleus.se
     and type 'a unit_st = 'a Nucleus.se
     and type ('a,'b) code_id = ('a,'b) Nucleus.code_id
     and type 'a data_id = 'a Nucleus.data_id

  module Chain : sig

    (** Specific to the functional implementation: Run an instruction sequence on a starting environment *)
    val execute : 'a Nucleus.se -> 'a option
    include C.Chain
      with type 'a st = 'a Nucleus.se
       and type 'a unit_st = 'a Nucleus.se
       and type ('a,'b) code_id = ('a,'b) Nucleus.code_id
       and type 'a data_id = 'a Nucleus.data_id
  end
end

(** Given a context, a command of type [('a -> 'b) st] returns an ['a -> 'b] function. In the imperative version, the context is globally accessible, so 
    [('a -> 'b) st = 'a -> 'b]. Unlike with [Fun], the [unit_st] type is not a synonym of [st]. Consider an [int st] command. In the functional version, it is actually of type [context -> (... int ...)]. In the imperative version, [int st = int]. To delay execution of nonfunctional command, we specify the type ['a unit_st = unit -> 'a]. That way, a command [c:int unit_st] must be invoked with [c ()]. *)
module Imp : sig
  module Program : C.Program
    with type 'a st := 'a 
     and type 'a unit_st := unit -> 'a
     and type ('a,'b) code_id = ('a,'b) Nucleus.code_id
     and type 'a data_id = 'a Nucleus.data_id

  module Chain : sig
    val initialize_chain : unit -> unit
    include C.Chain
      with type 'a st := 'a 
       and type 'a unit_st := unit -> 'a
       and type ('a,'b) code_id = ('a,'b) Nucleus.code_id
       and type 'a data_id = 'a Nucleus.data_id
  end

end
