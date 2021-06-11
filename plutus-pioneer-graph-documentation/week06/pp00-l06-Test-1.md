#### id: [[pp00-l06-Test-1]]
## Contract: [`Test.hs`](https://github.com/input-output-hk/plutus-pioneer-program/blob/main/code/week06/src/Week06/Oracle/Test.hs)

---
This is the Plutus Pioneer first ever course. Thaught by [Dr. Lars Brünjes](https://github.com/brunjlar). Find this lecture on [Youtube](https://www.youtube.com/watch?v=wY7R-PJn66g&t=4865s) and [Github](https://github.com/input-output-hk/plutus-pioneer-program/tree/main/code/week06).
---

---
This is the fourth contract of Lecture 6. Refer to [[pp00-l06]]
---

Set conditions to simulate a swap transaction. This contract makes use of the three previous contracts in Lecture06: [`Core.hs`](https://github.com/input-output-hk/plutus-pioneer-program/blob/main/code/week06/src/Week06/Oracle/Core.hs)[[pp00-l06-Core-1]], [`Swap.hs`](https://github.com/input-output-hk/plutus-pioneer-program/blob/main/code/week06/src/Week06/Oracle/Swap.hs)[[pp00-l06-Swap-1]] & [`Funds.hs`](https://github.com/input-output-hk/plutus-pioneer-program/blob/main/code/week06/src/Week06/Oracle/Funds.hs)[[pp00-l06-Funds-1]]

We give all actors 100 000 000 USDT
-   `checkOracle` checks the Orcale's quote recursively every slot
-   `myTrace`   is typical trace intiation stuff
    -   Define `OracleParams` for creation of the Oracle
    -   Start the Oracle for `Wallet 1` using `getOracle` [[pp00-l06-Test-1a]] and get the Oracle  value `oracle`
    -   Start `Wallet 2` and check, print the Oracle value at every slot
    -   `"update"` initiallize the Oracle to 1.5 USDT per ADA
    -   Check initial balances of Wallets 1 to 5
    -   Start the swap for Wallets 3,4,5

**Test Scenario.-** 

-   Wallet 3 offers 10 ADA & Wallet 4 offers 20 ADA
-   Wallet 5 uses the the swap that finds first among the last offers
-  Then Wallet 1 updates the Oracle value to 1.7 USDT/ADA
-  Wall et 5 does another swap with the updated quote
-  Wallet 1 updates the value again to 1.8 USDT/ADA
-  Wallets 3 & 4 retrieve their swaps, (although this has no effect because these were the only swaps available and were used at some point)
  
Run and try it on the Playground/Emulator.

*One extra detail here is that we are using `runEmulatorTraceIO'` that takes more arguments but allows finer control of the Emulator log messages*


## Full contract

```haskell
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Week06.Oracle.Test where

import           Control.Monad              hiding (fmap)
import           Control.Monad.Freer.Extras as Extras
import           Data.Default               (Default (..))
import qualified Data.Map                   as Map
import           Data.Monoid                (Last (..))
import           Data.Text                  (Text)
import           Ledger
import           Ledger.Value               as Value
import           Ledger.Ada                 as Ada
import           Plutus.Contract            as Contract hiding (when)
import           Plutus.Trace.Emulator      as Emulator
import           PlutusTx.Prelude           hiding (Semigroup(..), unless)
import           Prelude                    (Semigroup(..))
import           Wallet.Emulator.Wallet

import           Week06.Oracle.Core
import           Week06.Oracle.Funds
import           Week06.Oracle.Swap

assetSymbol :: CurrencySymbol
assetSymbol = "ff"

assetToken :: TokenName
assetToken = "USDT"

test :: IO ()
test = runEmulatorTraceIO' def emCfg myTrace
  where
    emCfg :: EmulatorConfig
    emCfg = EmulatorConfig $ Left $ Map.fromList [(Wallet i, v) | i <- [1 .. 10]]

    v :: Value
    v = Ada.lovelaceValueOf                    100_000_000 <>
        Value.singleton assetSymbol assetToken 100_000_000

checkOracle :: Oracle -> Contract () BlockchainActions Text a
checkOracle oracle = do
    m <- findOracle oracle
    case m of
        Nothing        -> return ()
        Just (_, _, x) -> Contract.logInfo $ "Oracle value: " ++ show x
    Contract.waitNSlots 1 >> checkOracle oracle

myTrace :: EmulatorTrace ()
myTrace = do
    let op = OracleParams
                { opFees = 1_000_000
                , opSymbol = assetSymbol
                , opToken  = assetToken
                }

    h1 <- activateContractWallet (Wallet 1) $ runOracle op
    void $ Emulator.waitNSlots 1
    oracle <- getOracle h1

    void $ activateContractWallet (Wallet 2) $ checkOracle oracle

    callEndpoint @"update" h1 1_500_000
    void $ Emulator.waitNSlots 3

    void $ activateContractWallet (Wallet 1) ownFunds'
    void $ activateContractWallet (Wallet 3) ownFunds'
    void $ activateContractWallet (Wallet 4) ownFunds'
    void $ activateContractWallet (Wallet 5) ownFunds'

    h3 <- activateContractWallet (Wallet 3) $ swap oracle
    h4 <- activateContractWallet (Wallet 4) $ swap oracle
    h5 <- activateContractWallet (Wallet 5) $ swap oracle

    callEndpoint @"offer" h3 10_000_000
    callEndpoint @"offer" h4 20_000_000
    void $ Emulator.waitNSlots 3

    callEndpoint @"use" h5 ()
    void $ Emulator.waitNSlots 3

    callEndpoint @"update" h1 1_700_000
    void $ Emulator.waitNSlots 3

    callEndpoint @"use" h5 ()
    void $ Emulator.waitNSlots 3

    callEndpoint @"update" h1 1_800_000
    void $ Emulator.waitNSlots 3

    callEndpoint @"retrieve" h3 ()
    callEndpoint @"retrieve" h4 ()
    void $ Emulator.waitNSlots 3
  where
    getOracle :: ContractHandle (Last Oracle) OracleSchema Text -> EmulatorTrace Oracle
    getOracle h = do
        l <- observableState h
        case l of
            Last Nothing       -> Emulator.waitNSlots 1 >> getOracle h
            Last (Just oracle) -> Extras.logInfo (show oracle) >> return oracle
```


---
2021-06-10 12:09
#### in [[pp00-l06]]

#swap #lecture06 #test