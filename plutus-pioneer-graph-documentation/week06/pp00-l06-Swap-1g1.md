#### id [[pp00-l06-Swap-1g1]]
---
Upon finding the first swap that we can afford, we take it* `Just (oref', o', pkh')`, then:
-   `v` output value for the Oracle (NFT+fees)
-   `p` price to pay (converted to type `Value`)
-   `lookups` allow to link the `otherScript`s that will be needed (2 scripts, one for the Oracle, another for the swap)
-   `tx` takes three `Constraints`:
    -   i) `mustSpendScriptOutput` uses the Oracle as input and then we use the `Redeemer` to consume the Oracle input
    -   ii) Consume the swap input, redeemer in this case is void `()`
    -   iii) `mustPayToOtherScript` pays value `v` to the Oracle script, consuming the current quote in Datum & `mustPayToPubKey` pays swap-price `p` to the Oracle

*Note.- this is not very realistic. Generally we would like to check more Oracle quotes to chose a the better offer for a swap*
```haskell
useSwap :: forall w s. HasBlockchainActions s => Oracle -> Contract w s Text ()
                    .
                    .
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
                    .
                    .
```

---
2021-06-05 15:24
#### in [[pp00-l06-Swap-1]]

#plutus-pioneer #lecture06 #swap 