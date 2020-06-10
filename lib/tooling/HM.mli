(** Heteregenous map. Uses the [Hmap] package. *)

(** Type of the information contained in an identifier *)
type 'a key_info = {
  name: string; (** Display name *)
  pp: (Format.formatter -> 'a -> unit) option; (** Optional pretty-print fn *)
  hidden:bool; (** Don't display at all during logging *)
  internal:bool; (** For code keys only. Prevent calling from an external contract. *)
}

(** Redefine Hmaps to work with the key_info type *)
include Hmap.S with type 'a Key.info = 'a key_info

(** Bindings are ('a key, 'a) pairs *)
val pp_binding : Format.formatter -> binding -> unit

val pp : Format.formatter -> t -> unit
