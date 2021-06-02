#### id [[pp00-l06-Core-1a]]
--- 

Create custom data-type `Oracle` & make it liftable. Also create custom `OracleRedeemer` to be able to tell a Tx whether to consume or update its **Datum**

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