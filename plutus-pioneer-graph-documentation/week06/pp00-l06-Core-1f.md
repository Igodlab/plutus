#### id [[pp00-l06-Core-1f]]
---

Typical boiler-plate actions:
-   Template Haskell for compilation into a Script instance.
-   Use lift code + wrappers, since we are using a parameterized type for the `oracle`
-   `oracleAddress` gets us a `Ledger.Address` from an `Oracle` data-type.
-   
```haskell
data Oracling
instance Scripts.ScriptType Oracling where
    type instance DatumType Oracling = Integer
    type instance RedeemerType Oracling = OracleRedeemer

oracleInst :: Oracle -> Scripts.ScriptInstance Oracling
oracleInst oracle = Scripts.validator @Oracling
    ($$(PlutusTx.compile [|| mkOracleValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode oracle)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @Integer @OracleRedeemer

oracleValidator :: Oracle -> Validator
oracleValidator = Scripts.validatorScript . oracleInst

oracleAddress :: Oracle -> Ledger.Address
oracleAddress = scriptAddress . oracleValidator
```

---
2021-06-06 14:00
#### in [[pp00-l06-Swap-1]]

#plutus-pioneer #lecture06 #core 