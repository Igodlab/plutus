#### id [[pp00-l06-Core-1e2]]
--- 

`validOutputDatum` checks that oracle validator has datum

```haskell
where
    .
    .
    .
    outputDatum :: Maybe Integer
    outputDatum = oracleValue ownOutput (`findDatum` info)

    validOutputDatum :: Bool
    validOutputDatum = isJust outputDatum
    .
    .
    .
```

---
#### in [[pp00-l06-Core-1e]]

#plutus-pioneer #core #lecture06 