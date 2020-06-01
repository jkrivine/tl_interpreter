open Env.Imp.Program

(* To be implement by inheritors *)
let commit : (Dec.parties,unit) code_identifier = code ()
let pull : (Dec.parties,unit) code_identifier = code ()
let on_connect : (Dec.parties,unit) code_identifier = code ()
let grow = code ()

let construct dec =
  code_set grow
    begin fun ((s,t),contract,pos_name) ->
      call dec Dec.Legal.grow ((s,t),contract,pos_name,get_caller ())
    end

