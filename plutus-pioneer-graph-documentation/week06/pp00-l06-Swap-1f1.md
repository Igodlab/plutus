#### id [[pp00-l06-Swap-1f1]]
---
Retrieving all UTXOs from the triplet: 

(Oracle's reference address, UTXO itself, Datum) = `(TxOutRef, TxOutTx, PubKeyHash)`

-   concatenate things...


```haskell
  let
    .
    .
    tx      = mconcat [Constraints.mustSpendScriptOutput oref $ Redeemer $ PlutusTx.toData () | (oref, _, _) <- xs]
    .
    .
```

---
2021-06-04 19:17
#### in [[pp00-l06-Swap-1f]]

#plutus-pioneer #lecture06 #swap 