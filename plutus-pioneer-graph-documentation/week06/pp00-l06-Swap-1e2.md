#### id [[pp00-l06-Swap-1e2]]
---

`guard :: Alternative f => Bool -> f ()` is in the Maybe Monad and it checks if the public-key-hash satisfies the predicate `p`. If it suceeds it returns the triplet: 
-   (Oracle's reference address, UTXO itself, Datum) =`(TxOutRef, TxOutTx, PubKeyHash)`
However, if it fails it returns Nothin (`f ()`), and then when used to map UTXOs into list it will drop the corresponding UTXO.

```haskell
.
.
  where
    f :: TxOutTx -> Maybe PubKeyHash
    f o = do
        dh        <- txOutDatumHash $ txOutTxOut o
        (Datum d) <- Map.lookup dh $ txData $ txOutTxTx o
        PlutusTx.fromData d

    g :: (TxOutRef, TxOutTx) -> Maybe (TxOutRef, TxOutTx, PubKeyHash)
    g (oref, o) = do
        pkh <- f o
        guard $ p pkh
        return (oref, o, pkh)
```

---
2021-06-04 11:30
#### in [[pp00-l06-SWap-1e2]]

#plutus-pioneer #lecture06 #swap 