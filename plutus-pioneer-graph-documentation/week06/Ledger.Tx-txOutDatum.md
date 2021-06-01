#### IOHK plutus

[[Ledger.Tx-txOutDatum]] defined as [`txOutDatum`](https://github.com/input-output-hk/plutus/blob/master/plutus-ledger-api/src/Plutus/V1/Ledger/Tx.hs)


```haskell
-- | The datum attached to a 'TxOutOf', if there is one.
txOutDatum :: TxOut -> Maybe DatumHash
txOutDatum TxOut{txOutDatumHash} = txOutDatumHash
```