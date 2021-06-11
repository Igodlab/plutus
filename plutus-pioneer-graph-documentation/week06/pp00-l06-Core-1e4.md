#### tag [[pp00-l06-Core-1e4]]
---

Verifiy that fees (+tip if whished) are paid to the oracle provider, the Tx output value has to be equal or greater than the `oFee` value set by the oracle.

```haskell
where
    .
    .
    feesPaid :: Bool
    feesPaid =
      let
        inVal  = txOutValue ownInput
        outVal = txOutValue ownOutput
      in
        outVal `geq` (inVal <> Ada.lovelaceValueOf (oFee oracle))
```


---
#### in [[pp00-l06-Core-1e]]

#plutus-pioneer #core #lecture06 