#### id [[pp00-l06-Swap-1e1]]
---
`mapMaybe` is a function that maps a function to every element on the input list and returns only the `Just` values. Try it on the repl:

```haskell
repl$ import Data.Maybe
repl$ :t mapMaybe
mapMaybe :: (a -> Maybe b) -> [a] -> [b]

```

One example of using `mapMaybe` is

```haskell
repl$ :set -XScopedTypeVariables
repl$ f (n :: Int) = if even n then just (div n 2) else Nothing
repl$ mapMaybe f $ take 20 [-5..]
[-2,-1,0,1,2,3,4,5,6,7]
```

---
2021-06-03 16:49
#### in [[pp00-l06-Swap-1e]]

#plutus-pioneer #lecture06 #swap 