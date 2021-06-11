#### id [[pp00-l06-Swap-1g]]
---

To `useSwap` we invoke the oracle
- `funds` handle looks-up for our `ownFunds` (imported from [`Funds.hs`](https://github.com/input-output-hk/plutus-pioneer-program/blob/main/code/week06/src/Week06/Oracle/Funds.hs)  [[pp00-l06-Funds-1]])
- `amt` checks how many tokens relevant to the oracle's quote we have, then log this information
- `m` handle gets us the triplet `findOracle` [[pp00-l06-Core-1i1]] that references the oracle's UTXO + the echange rate `x`
    - If this succeeds, we search for all the `swaps` available that are not owned by us (`/= pkh`)
    - `find` (defined in Prelude) gets the swaps that satisfy `f`, which checks that we own at least the same amount of tokens as the exchange price from the quote
        - If this goes trhough we proceed with executing the transaction, as usual use lookups, submit transaction, await for confirmation and set logs [[pp00-l06-Swap-1g1]].

```haskell
useSwap :: forall w s. HasBlockchainActions s => Oracle -> Contract w s Text ()
useSwap oracle = do
    funds <- ownFunds
    let amt = assetClassValueOf funds $ oAsset oracle
    logInfo @String $ "available assets: " ++ show amt

    m <- findOracle oracle
    case m of
        Nothing           -> logInfo @String "oracle not found"
        Just (oref, o, x) -> do
            logInfo @String $ "found oracle, exchange rate " ++ show x
            pkh   <- pubKeyHash <$> Contract.ownPubKey
            swaps <- findSwaps oracle (/= pkh)
            case find (f amt x) swaps of
                Nothing                -> logInfo @String "no suitable swap found"
                Just (oref', o', pkh') -> do
                    let v       = txOutValue (txOutTxOut o) <> lovelaceValueOf (oFee oracle)
                        p       = assetClassValue (oAsset oracle) $ price (lovelaces $ txOutValue $ txOutTxOut o') x
                        lookups = Constraints.otherScript (swapValidator oracle)                     <>
                                  Constraints.otherScript (oracleValidator oracle)                   <>
                                  Constraints.unspentOutputs (Map.fromList [(oref, o), (oref', o')])
                        tx      = Constraints.mustSpendScriptOutput oref  (Redeemer $ PlutusTx.toData Use) <>
                                  Constraints.mustSpendScriptOutput oref' (Redeemer $ PlutusTx.toData ())  <>
                                  Constraints.mustPayToOtherScript
                                    (validatorHash $ oracleValidator oracle)
                                    (Datum $ PlutusTx.toData x)
                                    v                                                                      <>
                                  Constraints.mustPayToPubKey pkh' p
                    ledgerTx <- submitTxConstraintsWith @Swapping lookups tx
                    awaitTxConfirmed $ txId ledgerTx
                    logInfo @String $ "made swap with price " ++ show (Value.flattenValue p)
  where
    getPrice :: Integer -> TxOutTx -> Integer
    getPrice x o = price (lovelaces $ txOutValue $ txOutTxOut o) x

    f :: Integer -> Integer -> (TxOutRef, TxOutTx, PubKeyHash) -> Bool
    f amt x (_, o, _) = getPrice x o <= amt
```

---
2021-06-05 13:18
#### in [[pp00-l06-Swap-1]]

#plutus-pioneer #lecture06 #swap 