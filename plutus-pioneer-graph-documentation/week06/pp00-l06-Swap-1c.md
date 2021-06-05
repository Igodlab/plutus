#### id [[pp00-l06-Swap-1c]]
---
Usual Boiler plate actions: instances + template Haskell
- instances records the Datum and redeemer
- `swapInst` template Haskell for compilation, using splices etc... 
- get validator `swapValidator` & get the address `swapAddress`

```haskell
data Swapping
instance Scripts.ScriptType Swapping where
    type instance DatumType Swapping = PubKeyHash
    type instance RedeemerType Swapping = ()

swapInst :: Oracle -> Scripts.ScriptInstance Swapping
swapInst oracle = Scripts.validator @Swapping
    ($$(PlutusTx.compile [|| mkSwapValidator ||])
        `PlutusTx.applyCode` PlutusTx.liftCode oracle
        `PlutusTx.applyCode` PlutusTx.liftCode (oracleAddress oracle))
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @PubKeyHash @()

swapValidator :: Oracle -> Validator
swapValidator = Scripts.validatorScript . swapInst

swapAddress :: Oracle -> Ledger.Address
swapAddress = scriptAddress . swapValidator
```

---
2021-06-03 14:08
#### in [[pp00-l06-Swap-1]]

#plutus-pioneer #lecture06 #swap 