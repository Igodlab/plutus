#### id [[pp00-l06-Swap-1b]]
---

We start by creating the `mkSwapValidator`.
The inputs are 1) `Oracle`, which we custom-defined it in [`Core.hs`](https://github.com/input-output-hk/plutus-pioneer-program/blob/main/code/week06/src/Week06/Oracle/Core.hs). 3) Datum is the `PubKeyHash` of the seller. 4) The redemer is type unit `()`. 5) Just context for the script. For this function to run we either need to met one of the two conditions:
- 1) Seller signing the transaction (`txSignedBy info pkh`)
- 2) Or if the contract finds exactly two script inputs `hasTwoScripInputs` [[pp00-l06-Swap-1b1]] that match the conditions for the swap `sellerPaid` [[pp00-l06-Swap-1b2]].

```haskell
{-# INLINABLE mkSwapValidator #-}
mkSwapValidator :: Oracle -> Address -> PubKeyHash -> () -> ScriptContext -> Bool
mkSwapValidator oracle addr pkh () ctx =
    txSignedBy info pkh ||
    (traceIfFalse "expected exactly two script inputs" hasTwoScriptInputs &&
     traceIfFalse "price not paid"                     sellerPaid)

  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    oracleInput :: TxOut
    oracleInput =
      let
        ins = [ o
              | i <- txInfoInputs info
              , let o = txInInfoResolved i
              , txOutAddress o == addr
              ]
      in
        case ins of
            [o] -> o
            _   -> traceError "expected exactly one oracle input"

    oracleValue' = case oracleValue oracleInput (`findDatum` info) of
        Nothing -> traceError "oracle value not found"
        Just x  -> x

    hasTwoScriptInputs :: Bool
    hasTwoScriptInputs =
      let
        xs = filter (isJust . toValidatorHash . txOutAddress . txInInfoResolved) $ txInfoInputs info
      in
        length xs == 2

    minPrice :: Integer
    minPrice =
      let
        lovelaceIn = case findOwnInput ctx of
            Nothing -> traceError "own input not found"
            Just i  -> lovelaces $ txOutValue $ txInInfoResolved i
      in
        price lovelaceIn oracleValue'

    sellerPaid :: Bool
    sellerPaid =
      let
        pricePaid :: Integer
        pricePaid =  assetClassValueOf (valuePaidTo info pkh) (oAsset oracle)
      in
        pricePaid >= minPrice
```

---
2021-06-2 08:27
#### in [[pp00-l06-Swap-1]]

#plutus-pioneer #lecture06 #swap 