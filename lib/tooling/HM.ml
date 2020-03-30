(*
 * Hmap-related stuff
 *)

(* Type of the information contained in an hkey *)
type 'a key_info = {
  name: string;
  pp: (Format.formatter -> 'a -> unit) option;
  init: 'a option;
  hidden: bool;
}

(* Redefine Hmaps to work with the key_info type *)
include Hmap.Make(struct type 'a t = 'a key_info end)

(* Bindings are ('a key, 'a) pairs *)
let pp_binding fmt (B (k,v)) =
  let {name;pp;hidden;_} = Key.info k in
  if hidden then ()
  else begin
    F.cr (); match pp with
    | None -> F.p fmt "%s ↦  <opaque>" name
    | Some f ->
      F.p fmt "%s ↦  " name;
      F.with_indent (fun () -> F.p fmt "%a" f v)
  end

let pp fmt hmap =
  if cardinal hmap = 0 then(
    F.p fmt "<empty hmap>"
  )else (
    F.with_indent (fun () -> iter (fun b -> pp_binding fmt b) hmap)
  )
