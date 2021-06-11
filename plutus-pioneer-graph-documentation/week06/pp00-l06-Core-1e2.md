#### id [[pp00-l06-Core-1e2]]
--- 

`validOutputDatum` checks that oracle validator has datum. In this contract datum is the integer value of the exchange price
```haskell
where
    .
    .
    outputDatum :: Maybe Integer
    outputDatum = oracleValue ownOutput (`findDatum` info)

    validOutputDatum :: Bool
    validOutputDatum = isJust outputDatum
    .
    .
```


---
#### in [[pp00-l06-Core-1e]]

#plutus-pioneer #core #lecture06 