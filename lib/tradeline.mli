include module type of Tradeline_types

(**[transfer tl p player] transfer ownership of position [p] to [player]*)
val transfer : tradeline -> pos -> addr option -> tradeline

val ownerOf : tradeline -> pos -> addr option

(**[provision tl p player a] provisions [a] coins to segment after position [p] for [player], NB(ext) anyone can call*)
val provision : tradeline -> pos -> amount -> tradeline

(**[reduce tl p Seller t c] Backward reduction of [p] at time [t] on clause [c]. NB(ext) caller should be owner of [p]
[reduce tl p Buyer t c] Forward reduction of [p+] at time [t] on clause [c].
NB(ext) caller should be owner of [p+]
*)
val reduce : tradeline -> pos -> side -> time -> clause -> (tradeline*payoff)

val init : addr -> tradeline

val grow: tradeline -> segment -> tradeline

(* Throws if given provision has no segment *)
(* Provisioning a dead, gc-able provision is fine *)



