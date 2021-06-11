#### id [[pp00-l06-Test-1a]]
---
Because of the reason that the `runOracle` [[pp00-l06-Core-1j]] contract is parameterized over the Oracle value and as soon it is created it **tells** (`tells :: w -> Contract w s e ()` ) the contract

`getOracle` hyperfunction gets the state of the Oracle upon creation, if not it waits

```haskell
.
.
  where
    getOracle :: ContractHandle (Last Oracle) OracleSchema Text -> EmulatorTrace Oracle
    getOracle h = do
        l <- observableState h
        case l of
            Last Nothing       -> Emulator.waitNSlots 1 >> getOracle h
            Last (Just oracle) -> Extras.logInfo (show oracle) >> return oracle
```

---
2021-06-10 16:53
#### in [[pp00-l06-Test-1]]

#plutus-pioneer #lecture06 #test 