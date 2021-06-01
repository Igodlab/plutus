#### id [[pp01-l06-Core-1c]]
Creates the proper type: `AssetClass` [[Ledger.Value-AssetClass]] for the Oracle.

```haskell
{-# INLINABLE oracleAsset #-}
oracleAsset :: Oracle -> AssetClass
oracleAsset oracle = AssetClass (oSymbol oracle, oracleTokenName)
```
---
#### in [[pp00-l06-Core-1]]

#plutus-pioneer #lecture06 