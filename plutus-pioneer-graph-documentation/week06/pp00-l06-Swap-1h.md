#### id [[pp00-l06-Swap-1h]]
---

Define `SwapScheema` and endpointds to interact with the Playground/Emulator 
We call `swap` recursively to offer `IO` actions with the endpoints:
-   `offer`, `retrieve` and `use` are blocked endpoints until they are called from the outside.
-   `funds` blocked until `"funds"` endpoint is called, and report the funds available `v` [[pp00-l06-Funds-1]]
If something along the way goes wrong, the error handle `h` will log an error and continue instead of letting everything crash. 

```haskell
type SwapSchema =
    BlockchainActions
        .\/ Endpoint "offer"    Integer
        .\/ Endpoint "retrieve" ()
        .\/ Endpoint "use"      ()
        .\/ Endpoint "funds"    ()

swap :: Oracle -> Contract (Last Value) SwapSchema Text ()
swap oracle = (offer `select` retrieve `select` use `select` funds) >> swap oracle
  where
    offer :: Contract (Last Value) SwapSchema Text ()
    offer = h $ do
        amt <- endpoint @"offer"
        offerSwap oracle amt

    retrieve :: Contract (Last Value) SwapSchema Text ()
    retrieve = h $ do
        endpoint @"retrieve"
        retrieveSwaps oracle

    use :: Contract (Last Value) SwapSchema Text ()
    use = h $ do
        endpoint @"use"
        useSwap oracle

    funds :: Contract (Last Value) SwapSchema Text ()
    funds = h $ do
        endpoint @"funds"
        v <- ownFunds
        tell $ Last $ Just v

    h :: Contract (Last Value) SwapSchema Text () -> Contract (Last Value) SwapSchema Text ()
    h = handleError logError
```

---
2021-06-10 11:50
#### in [[pp00-l06-Swap-1]]

#plutus-pioneer #lecture06 #swap 