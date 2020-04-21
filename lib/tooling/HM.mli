(*
 * Hmap-related stuff
 *)

(* Type of the information contained in an hkey *)
type 'a key_info = {
  name: string;
  pp: (Format.formatter -> 'a -> unit) option;
  hidden:bool;
}

(* Redefine Hmaps to work with the key_info type *)
include Hmap.S with type 'a Key.info = 'a key_info

(* Bindings are ('a key, 'a) pairs *)
val pp_binding : Format.formatter -> binding -> unit

val pp : Format.formatter -> t -> unit
