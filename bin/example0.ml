open Imperative

(* Loading data for example 0 *)
open Data0


let () =
  (*initialize_example ();*)
  C.echo
"║ Example 0
╠═══════════
║ We are in an idealized setting, with a 'chain' that is just a sequence of
║ state modifications and 'programs' stored in that state that can be invoked for
║ further modifications.
║
║ In this first example, we transfer tokens and create a new contract.
║
║ `open Imperative` at line 1 of the source file puts a module `C` in scope.
║ C stands for Chain. It provides an interface for acting on the chain itself.
║ C contains functions for extending the chain with new transactions,
║ manipulating the state globally (time modification and state save/restore), and
║ general printing functions
║
║ Here is the current state of the blockchain:
";

  C.echo_state ();

  C.echo "║
║ In the current state, two contracts exist: dollar and euro. They each contain a
║ ledger assigning an amount to their existing users. There are two users: alice
║ and bob. alice owns $11 and €9, while bob just owns €9.  Since bob has never
║ held a balance at the euro contract, it is not mentioned in euro's ledger.
║
║ alice will now send $10 to bob. We represent this event by adding a transaction
║ using `C.tx`
║
║ Evaluating `C.tx usr ctr meth args` adds a new transaction to the chain.
║
║ That transaction will:
║   * originate from `usr`
║   * call the method `meth` of contract `ctr` with arguments `args`.
║
║ We now execute the transaction with
║
║   ┌───────────────────────────────────────────┐
║   │ C.tx alice dollar Token.transfer (10,bob) │
║   └───────────────────────────────────────────┘
";

  C.tx alice dollar Token.transfer (10,bob);
  C.echo "║
║ alice has now 10 dollars less, and bob 10 more. We can always see the current
║ state using
║
║   ┌─────────────────┐
║   │ C.echo_state () │
║   └─────────────────┘
";

  C.echo_state ();

  C.echo "║
║ Any error encountered during a transaction originated by `C.tx` reverts to the
║ previous state; further transactions still execute. For instance, alice will
║ now try to send $2 to bob with the following code:
║
║   ┌──────────────────────────────────────────┐
║   │ C.tx alice dollar Token.transfer (2,bob) │
║   └──────────────────────────────────────────┘
";
  C.tx alice dollar Token.transfer (2,bob);

  C.echo "
║ After the error, the state is the same as before:
";

  C.echo_state ();

  C.echo "║
║ We can also directly get alice's current balance to use it in further
║ transactions.  In our setting, this means originating a special
║ `C.txr` which returns whatever value the contract call returns.
║ `C.tx` ignores that return value.  Note that an error encountered during
║ `C.txr` will interrupt the flow of transactions.
║
║ We assign alice's balance to a local variable with the following let ... in :
║
║   ┌───────────────────────────────────────────────────────────────┐
║   │ let alice_balance = C.txr alice dollar Token.balance alice in │
║   └───────────────────────────────────────────────────────────────┘
";

  let alice_balance = C.txr alice dollar Token.balance alice in

  C.echo ("║ Alice now has: "^(string_of_int alice_balance)^" dollars");
  C.echo "║ (The above line was evaluated at runtime using the variable `alice_balance`)
║
║ To conclude this example, we demonstrate contract creation.
║
║ `C.tx_create usr name meth args` adds a contract-creating transaction to the
║ chain.
║   * The originator is `usr`
║   * The contract name (for printing purposes) is `name`
║   * The contents of the contract is the result of invoking `meth` at a fresh
║     address with arguments `args'.
║
║ alice will create a new instance of the Token contract (dollar and euro are
║ both instances of Token) with the following code:
║
║   ┌───────────────────────────────────────────────────────────────────┐
║   │ let galleons = C.tx_create alice \"galleons\" Token.construct () in │
║   └───────────────────────────────────────────────────────────────────┘
";

  let galleons = C.tx_create alice "galleons" Token.construct () in

  C.echo_state ();
  C.echo "║
║ The state now contains a new contract, galleons, with own balance (currently
║ empty), and an owner. A Token contract remembers who created it and gives it
║ admin rights; in particular it can call the `mint_for` function which creates
║ tokens out of thin air and gives them to someone.
║
║ alice will now give a lot of galleons to bob using the following:
║
║   ┌──────────────────────────────────────────────────┐
║   │ C.tx alice galleons Token.mint_for (3948234,bob) │
║   └──────────────────────────────────────────────────┘
║";
  C.tx alice galleons Token.mint_for (3948234,bob);

  C.echo "║ Which results in bob having plenty of galleons:
";

  C.echo_state ()
