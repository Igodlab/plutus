#### id [[pp00-l06-Core-1e3]]
---

Check that the Tx inputs and outputs(`inputHasToken` & `outputHasToken`, respectively), contain the unique NFT identifier

```haskell
where
    .
    .
    .
    ownInput :: TxOut
    ownInput = case findOwnInput ctx of
        Nothing -> traceError "oracle input missing"
        Just i  -> txInInfoResolved i

    inputHasToken :: Bool
    inputHasToken = assetClassValueOf (txOutValue ownInput) (oracleAsset oracle) == 1

    ownOutput :: TxOut
    ownOutput = case getContinuingOutputs ctx of
        [o] -> o
        _   -> traceError "expected exactly one oracle output"

    outputHasToken :: Bool
    outputHasToken = assetClassValueOf (txOutValue ownOutput) (oracleAsset oracle) == 1
    .
    .
    .
```

---
#### in [[pp00-l06-Core-1e]]

#plutus-pioneer #lecture06 #core 