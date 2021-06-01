#### IOHK Plutus repo
[[Ledger.Value-AssetClass]] defined as [`AssetClass`](https://github.com/input-output-hk/plutus/blob/master/plutus-ledger-api/src/Plutus/V1/Ledger/Value.hs)

```haskell
-- | An asset class, identified by currency symbol and token name.
newtype AssetClass = AssetClass { unAssetClass :: (CurrencySymbol, TokenName) }
	deriving stock (Generic)
	deriving newtype (Haskell.Eq, Haskell.Ord, Haskell.Show, Eq, Ord, PlutusTx.IsData, Serialise)
	deriving anyclass (Hashable, NFData, ToJSON, FromJSON)
	deriving Pretty via (PrettyShow (CurrencySymbol, TokenName))
```