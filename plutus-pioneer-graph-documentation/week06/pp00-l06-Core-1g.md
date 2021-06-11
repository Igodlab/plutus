#### id [[pp00-l06-Core-1g]]
---
Construct parameters for the Oracle creation from the off-chain end: `opFees, opSymbol, opToken`

*Note.- There is no NFT here, because we will mint it together with the creation of the Oracle at* `startOracle`[[pp00-l06-Core-1h]] 

```haskell
data OracleParams = OracleParams
    { opFees   :: !Integer
    , opSymbol :: !CurrencySymbol
    , opToken  :: !TokenName
    } deriving (Show, Generic, FromJSON, ToJSON)
```

---
2021-06-09 19:08
#### in [[pp00-l06-Swap-1]]

#plutus-pioneer #lecture06 #core 