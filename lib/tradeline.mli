include module type of Tradeline_types

(**[transfer tl p player] transfer ownership of position [p] to [player]*)
val transfer : tradeline -> pos -> addr -> tradeline

(**[provision tl p player a] provisions [a] coins to segment after position [p] for [player], NB(ext) anyone can call*)
val provision : tradeline -> pos -> addr -> amount -> tradeline

(**[reduce tl p Seller t c] Backward reduction of [p] at time [t] on clause [c]. NB(ext) caller should be owner of [p]
[reduce tl p Buyer t c] Forward reduction of [p+] at time [t] on clause [c].
NB(ext) caller should be owner of [p+]
*)
val reduce : tradeline -> pos -> side -> time -> clause -> (tradeline * payoff)

val gc: tradeline -> pos -> side -> time -> pos option -> clause -> (tradeline*amount)

val init : addr -> tradeline

val grow: tradeline -> segment -> tradeline
val bind: tradeline -> asset -> tradeline
val unbind: tradeline -> (tradeline*asset option)



