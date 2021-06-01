#### id [[pp00-l06-Core-1d]]

Get data from datum. A more general case is to consider that Datum can be empty. Here, two intermediary steps were taken [[pp00-l06-Core-1d1]]

```haskell
{-# INLINABLE oracleValue #-}
oracleValue :: TxOut -> (DatumHash -> Maybe Datum) -> Maybe Integer
oracleValue o f = do
    dh      <- txOutDatum o
    Datum d <- f dh
    PlutusTx.fromData d
```

---
#### sub [[pp00-l06-Core-1]]


