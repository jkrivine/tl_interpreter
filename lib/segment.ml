open Env.Imp.Program

(* To be implement by inheritors *)
let commit : (Dec.parties,unit) code_identifier = code ()
let pull : (Dec.parties,unit) code_identifier = code ()
let grow = code ()

let construct dec =
  code_set grow
    begin fun (parties,contract,pos_name) ->
      call dec Dec.Legal.grow (parties,contract,pos_name,get_caller ())
    end

