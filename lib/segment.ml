(** {e Contract}. Implements basic Segment skeleton. *)
open Env.Imp.Program

(** To be implement by inheritors *)
let commit : (Dec.parties,unit) code_id = code ()

(** To be implement by inheritors *)
let pull : (Dec.parties,unit) code_id = code ()

(** To be implement by inheritors *)
let on_connect : (Dec.parties,unit) code_id = code ()

(** Transparently proxy all grow calls *)
let grow = code ()

let construct dec =
  code_set grow
    begin fun ((s,t),contract,pos_name) ->
      call dec Dec.Legal.grow ((s,t),contract,pos_name,get_caller ())
    end

