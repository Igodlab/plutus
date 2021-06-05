#### id [[pp00-l06-Swap-1a]]
---

Define Ada variables that will be used thoughout the contract:
- We start by setting the exchange price `price`.
- `lovelaces` gets the ammount of Ada (`Value -> Integer`)

```haskell
{-# INLINABLE price #-}
price :: Integer -> Integer -> Integer
price lovelace exchangeRate = (lovelace * exchangeRate) `divide` 1000000

{-# INLINABLE lovelaces #-}
lovelaces :: Value -> Integer
lovelaces = Ada.getLovelace . Ada.fromValue
```


--- 
#### in [[pp00-l06-Swap-1]]
2021-06-01 11:20
#swap #plutus-pioneer #lecture06 