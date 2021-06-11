#### id [[pp00-l06-Core-1a]]
--- 

This oracle will be used to quote swap prices. As usual it contains an NFT as unique identifier. To mint its NFT we need a token-name and currency-symbol

Here we create a custom data-type `Oracle` & make it liftable. Also create  `OracleRedeemer` to be able to tell a Tx whether to consume or update its **Datum**

*Note.- here `oAsset :: !AssetClass` refers to the swap-token and not to the NFT* 

```haskell
data Oracle = Oracle
    { oSymbol   :: !CurrencySymbol
    , oOperator :: !PubKeyHash
    , oFee      :: !Integer
    , oAsset    :: !AssetClass
    } deriving (Show, Generic, FromJSON, ToJSON, Prelude.Eq, Prelude.Ord)

PlutusTx.makeLift ''Oracle

data OracleRedeemer = Update | Use
    deriving Show

PlutusTx.unstableMakeIsData ''OracleRedeemer
```

---
#### in [[pp00-l06-Core-1]]
#plutus-pioneer  #lecture06 #core 