#### id [[pp00-l06-Swap-1f]]
---
`retrieveSwaps` makes possbile for the seller to change his mind, and retrieve his tokens. Takes the Oracle owner's public-key-hash as an input
-   `pkh` takes the owner's public-key-hash
-   Our hyperfunction `findSwaps`  [[pp00-l06-Swap-1e]] will get us all the UTXOs swaps, which is then reduced to the the swap of the original seller
-   If the previous step succeds we need to rerieve the triplet : (Oracle's reference address, UTXO itself, Datum) = `(TxOutRef, TxOutTx, PubKeyHash)`. This is done in `tx` (details [[pp00-l06-Swap-1f1]])
-   submit Tx, wait for confirmation and set log messages stating how many swaps were retrieved


```haskell
retrieveSwaps :: HasBlockchainActions s => Oracle -> Contract w s Text ()
retrieveSwaps oracle = do
    pkh <- pubKeyHash <$> ownPubKey
    xs <- findSwaps oracle (== pkh)
    case xs of
        [] -> logInfo @String "no swaps found"
        _  -> do
            let lookups = Constraints.unspentOutputs (Map.fromList [(oref, o) | (oref, o, _) <- xs]) <>
                          Constraints.otherScript (swapValidator oracle)
                tx      = mconcat [Constraints.mustSpendScriptOutput oref $ Redeemer $ PlutusTx.toData () | (oref, _, _) <- xs]
            ledgerTx <- submitTxConstraintsWith @Swapping lookups tx
            awaitTxConfirmed $ txId ledgerTx
            logInfo @String $ "retrieved " ++ show (length xs) ++ " swap(s)"
```

---
2021-06-04 17:56
#### in [[pp00-l06-Swap-1]]

#plutus-pioneer #lecture06 #swap 