include module type of Tradeline_types

(**[transfer tl p player] transfer ownership of position [p] to [player]*)
val transfer : t -> pos -> addr option -> t

val ownerOf : t -> pos -> addr option

(**[provision tl p player a] provisions [a] coins to segment after position [p] for [player], NB(ext) anyone can call*)
val provision : t -> pos -> amount -> t

(**[reduce tl p Seller t c] Backward reduction of [p] at time [t] on clause [c]. NB(ext) caller should be owner of [p]
[reduce tl p Buyer t c] Forward reduction of [p+] at time [t] on clause [c].
NB(ext) caller should be owner of [p+]
*)
val reduce : t -> Ledger.t -> pos -> side -> time -> clause -> (t * Ledger.t)

val init : addr -> t

val grow: t -> segment -> t

(* Throws if given provision has no segment *)
(* Provisioning a dead, gc-able provision is fine *)



