#### id [[pp00-l06-Core-1i1]]
---
`findOdacle` gets us the triplet: (Oracle's reference address, UTXO itself, current **data** from datum)=`(TxOutRef, TxOutTx, Integer)`
-   `utxos` gets the one and only address that contains the NFT as a Map
-   return the triplet `(oref, o, x)` where `x` is the datum.

```haskell
findOracle :: forall w s. HasBlockchainActions s => Oracle -> Contract w s Text (Maybe (TxOutRef, TxOutTx, Integer))
findOracle oracle = do
    utxos <- Map.filter f <$> utxoAt (oracleAddress oracle)
    return $ case Map.toList utxos of
        [(oref, o)] -> do
            x <- oracleValue (txOutTxOut o) $ \dh -> Map.lookup dh $ txData $ txOutTxTx o
            return (oref, o, x)
        _           -> Nothing
  where
    f :: TxOutTx -> Bool
    f o = assetClassValueOf (txOutValue $ txOutTxOut o) (oracleAsset oracle) == 1
```

---
2021-06-05 13:32
#### in [[pp00-l06-Core-1i]]

#plutus-pioneer #lecture06 #core 