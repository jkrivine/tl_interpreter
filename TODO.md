# TODO

* write standard contracts eg loan, cds, etc
* add notary
* add token/NFT adapters
* check "grow constraints" and multi-layer grow constraints

## Prettier-printing of tradelines

  - Simple line-based version, using Format for auto newlines
  - How to represent nested lines in text format?

## Implement Token-specific 'allowance' 
instead of the current stub based on transfer callback


# Questions :
* whats the point of having internal token ledgers if user can go out anytime?
   (apart from zcrossing, apart from unified interface that disregards ERC specifics)


# Ideas

  * remove import bullshit
  * replace get_caller with ge_tdata caller with caller a universal key 
  * check data_set on universal keys is always NO 
  * add special !ref syntax to look nicer 
  * remove all the private_data bullshit? internal is just out of module signature, private is not in make return value 



## Avoid having separate code declaration / implementation

Instead of doing `code ()` and then `code_set (fun...)` we could have just 

```
let c = X.code "name" (fun ...)
```

where `X` was generated on the fly and given as argument to the `Contract` functor. `X.code` binds the function to the current address. However note that in this case, there is no public handler for code?

```
call dec Dec.blabla
```

No? Because `Dec` is now a functor.
Also it is harded to do 'virtual' functions with callbacks? Wait no it's impossible?  Can I do

``
let a = code "bla" (fun bli)
let b = implement Token.callback (fun bla)

create (Dec(module new_module ()).constructor
        create (M: functor) ->
       M(new module).constructor

       let a = X.code "bla"  (fun ...)
       let b = X.implement "bli" Token.u
```
