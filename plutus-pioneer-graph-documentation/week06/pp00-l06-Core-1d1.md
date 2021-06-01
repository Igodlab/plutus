#### id [[pp00-l06-Core-1d1]]

Two steps:
- If datum is present in the tx-out, `dh` grabs the datum hash from `TxOut`using `txOutDatum`. Both defined in [`TxOut`, `txOutDatum`](https://github.com/input-output-hk/plutus/blob/master/plutus-ledger-api/src/Plutus/V1/Ledger/Tx.hs))
-  `Datum d` gets the datum hash and turns it into datum (we are using the monoid `f`)

---
#### in [[pp00-l06-Core-1d]]