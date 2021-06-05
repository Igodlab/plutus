#### id [[pp00-l06-Swap-1d]]
---

`OfferSwap` is the first contract that is intended for a Seller to make an offer. Its parameters are: 1) `Oracle`, 2) the amount of lovelace (`Integer`) that he wants to offer and returns a contract.
-   we start looking up for its public key in `pkh`
-   then create a `tx` that locks the amount `amt` to the script
-   submit the `tx` with `submitTxConstraints` and set log messages


```haskell
offerSwap :: forall w s. HasBlockchainActions s => Oracle -> Integer -> Contract w s Text ()
offerSwap oracle amt = do
    pkh <- pubKeyHash <$> Contract.ownPubKey
    let tx = Constraints.mustPayToTheScript pkh $ Ada.lovelaceValueOf amt
    ledgerTx <- submitTxConstraints (swapInst oracle) tx
    awaitTxConfirmed $ txId ledgerTx
    logInfo @String $ "offered " ++ show amt ++ " lovelace for swap"
```

---
2021-06-03 14:23
#### in [[pp00-l06-Swap-1]]

#plutus-pioneer #lecture06 #swap 