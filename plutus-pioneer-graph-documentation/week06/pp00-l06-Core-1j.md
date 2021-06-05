#### id [[pp00-l06-Core-1j]]
---

`runOracle` combines both `startOracle` & `updateOracle` to run everything as one smart contract. In addition, Endpoints for logs in the Playground are included

```haskell
type OracleSchema = BlockchainActions .\/ Endpoint "update" Integer

runOracle :: OracleParams -> Contract (Last Oracle) OracleSchema Text ()
runOracle op = do
    oracle <- startOracle op
    tell $ Last $ Just oracle
    go oracle
  where
    go :: Oracle -> Contract (Last Oracle) OracleSchema Text a
    go oracle = do
        x <- endpoint @"update"
        updateOracle oracle x
        go oracle
```

---
2021-05-27 12:24
#### in [[pp00-l06-Core-1j]]
#plutus-pioneer #lecture06 #core 