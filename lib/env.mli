module Nucleus : sig
  (*type storage*)
  (*type state*)
  (*type context*)
  (*type env*)

  type 'a st
  type ('a,'b) code_hkey
  type 'a data_hkey

  (*val empty_state : state*)

  (*val empty_env : env*)
  (*val state_get : env -> state*)
  (*val state_set : env -> state -> env*)
  (*val context_get : env -> context*)

  (*val bind  : 'a st -> ('a -> 'b st) -> 'b st*)
  (*val (>>=) : 'a st -> ('a -> 'b st) -> 'b st*)
  (*val ( let* ) : 'a st -> ('a -> 'b st) -> 'b st*)
  (*val (>>) : 'a st -> 'b st -> 'b st*)
  (*val return : 'a -> 'a st*)
  (*val error : string -> 'a st*)
end

module Program : C.Program
  with type 'a st = 'a Nucleus.st 
   and type 'a unit_st = 'a Nucleus.st
   and type ('a,'b) code_hkey = ('a,'b) Nucleus.code_hkey
   and type 'a data_hkey = 'a Nucleus.data_hkey

module Chain : sig
  (* Run an instruction sequence on a starting environment *)
  (*val execute_from : 'a st -> env -> 'a option*)
  (* Run an instruction sequence on the empty environment *)
  val execute : 'a Nucleus.st -> 'a option
  include C.Chain
    with type 'a st = 'a Nucleus.st 
     and type 'a unit_st = 'a Nucleus.st
     and type ('a,'b) code_hkey = ('a,'b) Nucleus.code_hkey
     and type 'a data_hkey = 'a Nucleus.data_hkey
end

module Imp : sig
  module Program : C.Program
    with type 'a st := 'a 
     and type 'a unit_st := unit -> 'a
     and type ('a,'b) code_hkey = ('a,'b) Nucleus.code_hkey
     and type 'a data_hkey = 'a Nucleus.data_hkey

  module Chain : sig
    val initialize_chain : unit -> unit
    include C.Chain
      with type 'a st := 'a 
       and type 'a unit_st := unit -> 'a
       and type ('a,'b) code_hkey = ('a,'b) Nucleus.code_hkey
       and type 'a data_hkey = 'a Nucleus.data_hkey
  end

end
