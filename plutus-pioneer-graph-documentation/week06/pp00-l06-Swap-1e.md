#### id [[pp00-l06-Swap-1e]]
---
`findSwaps` is a helper function that finds all swaps that meet certain **predicate** `p`
-   `utxoAt` gives all the UTXOs sitting at the swap address as a `UtxoMap` 
-   we make use of `mapMaybe` to get all elements of the list-mapped swap addresses by a function `g` (more on `mapMaybe` [[Data.Maybe-mapMaybe]])
-   function `f` deserializes data into Datum (`PubKeyHash`), ifsuccesfull
-   function `g` maps all the UTXOs into the same UTXOs+Datum (details on function `f` and `g`[[pp00-l06-Swap-1e2]])

```haskell
findSwaps :: HasBlockchainActions s => Oracle -> (PubKeyHash -> Bool) -> Contract w s Text [(TxOutRef, TxOutTx, PubKeyHash)]
findSwaps oracle p = do
    utxos <- utxoAt $ swapAddress oracle
    return $ mapMaybe g $ Map.toList utxos
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

In this chunk of code the functions/variables used are
```haskell
utxoAt         :: (AsContractError e, HasUtxoAt s) => Address -> Contract w s e UtxoMap
txOutTxOut     :: TxOutTx -> TxOut
txOutDatumHash :: Maybe DatumHash
txOutTxTx      :: Tx
txData         :: Map DatumHash Datum`
guard          :: Alternative f => Bool -> f ()
```
---
2021-06-03 16:09
#### in [[pp00-l06-Swap-1]]

#plutus-pioneer #lecture06 #swap 