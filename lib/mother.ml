(**)

type mother_contract = {
    ledger : (addr, amount) MP.t;
}

val new_tl : unit -> addr
