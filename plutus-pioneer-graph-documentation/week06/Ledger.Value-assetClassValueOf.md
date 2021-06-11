#### IOHK Plutus repo
[[Ledger.Value-assetClassValueOf]] defined as [`assetClassValueOf`](https://github.com/input-output-hk/plutus/blob/master/plutus-ledger-api/src/Plutus/V1/Ledger/Value.hs)

```haskell
{-# INLINABLE assetClassValueOf #-}
-- | Get the quantity of the given 'AssetClass' class in the 'Value'.
assetClassValueOf :: Value -> AssetCLass -> Integer
assetClassValueOf v (AssetCLass (c, t)) = valueOf v c t
```