#### id [[pp00-l06-Swap-1b1]]
---
We filter all the script inputs and verify that there is only two

```haskell
    .
    .
    hasTwoScriptInputs :: Bool
    hasTwoScriptInputs =
      let
        xs = filter (isJust . toValidatorHash . txOutAddress . txInInfoResolved) $ txInfoInputs info
      in
        length xs == 2
    .
    .
```

In this chunk of code the functions used are:

```haskell
filter           :: (a -> Bool) -> [a] -> [a]
isJust           :: Maybe a -> Bool
toValidatorHash  :: Address -> Maybe a -> ValidatorHash
txOutAddress     :: TxOut -> Address
txInInfoResolved :: TxInInfo -> TxOut
txInfoInputs     :: txInfo -> [txInfo]
```


---
2021-06-02 08:46
#### in [[pp00-l06-Swap-1b]]

#plutus-pioneer #lecture06 #swap 