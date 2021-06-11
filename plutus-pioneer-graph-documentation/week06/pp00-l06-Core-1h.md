#### id [[pp00-l06-Core-1h]]
---
Starting the Oracle will imply both the creation of its unique NFT and the Oracle itself.

Rather than providing data for minting the NFT in `OracleParams`[[pp00-l06-Core-1g]]. Minting occurs in `startOracle` because of potential conflicting slot-delays in the minting process

```haskell
startOracle :: forall w s. HasBlockchainActions s => OracleParams -> Contract w s Text Oracle
startOracle op = do
    pkh <- pubKeyHash <$> Contract.ownPubKey
    osc <- mapError (pack . show) (forgeContract pkh [(oracleTokenName, 1)] :: Contract w s CurrencyError OneShotCurrency)
    let cs     = Currency.currencySymbol osc
        oracle = Oracle
            { oSymbol   = cs
            , oOperator = pkh
            , oFee      = opFees op
            , oAsset    = AssetClass (opSymbol op, opToken op)
            }
    logInfo @String $ "started oracle " ++ show oracle
    return oracle
```

---
2021-06-09 19:11
#### in [[pp00-l06-Swap-1]]

#plutus-pioneer #lecture06 #core 