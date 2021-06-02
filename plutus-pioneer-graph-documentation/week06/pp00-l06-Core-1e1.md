#### id [[pp00-l06-Core-1e1]]
----

Check whether the incoming Tx contains the Oracle's `PubKeyHash`

```haskell
.
.
.
case r of
    Update -> traceIfFalse "operator signature missing" (txSignedBy info $ oOperator oracle) &&
    .
    .
    .
```

---
#### in [[pp00-l06-Core-1e]]

#plutus-pioneer #lecture06 #core 