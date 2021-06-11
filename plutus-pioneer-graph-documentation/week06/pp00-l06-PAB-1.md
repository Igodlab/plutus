

#### id: [[pp00-l06-PAB-1]]
## Contract: [`PAB.hs`](https://github.com/input-output-hk/plutus-pioneer-program/blob/main/code/week06/src/Week06/Oracle/PAB.hs)

---
This is the Plutus Pioneer first ever course. Thaught by [Dr. Lars Brünjes](https://github.com/brunjlar). Find this lecture on [Youtube](https://www.youtube.com/watch?v=wY7R-PJn66g&t=4865s) and [Github](https://github.com/input-output-hk/plutus-pioneer-program/tree/main/code/week06).
---

---
This is the fifth contract of Lecture 6. Refer to [[pp00-l06]]
---

This is the **Plutus Application Backend** contract. Until the Testnet is ready we will deploy this as a **Mockchain**

The module PAB is just one data-type definition that defines the contract that we want to run 
-   `Init` will setup an environment with funds equivalently to how we would do it in the Emulator
-   `Oracle` constructor corresponds to the `runOracle` contract [[pp00-l06-Core-1j]], that starts, runs and gives `"update"` endpoint (also `currencySymbol` is the symbol that we will assign to USDT tokens)
-   `Swap` is parameterized by `Oracle.Oracle` and gives more endpoints [[pp00-l06-Swap-1h]]

## Full contract

```haskell
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}

module Week06.Oracle.PAB
    ( OracleContracts (..)
    ) where

import           Data.Aeson                (FromJSON, ToJSON)
import           Data.Text.Prettyprint.Doc (Pretty (..), viaShow)
import           GHC.Generics              (Generic)
import           Ledger

import qualified Week06.Oracle.Core        as Oracle

data OracleContracts = Init | Oracle CurrencySymbol | Swap Oracle.Oracle
    deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

instance Pretty OracleContracts where
    pretty = viaShow
```

---
2021-06-02 10:19
#### in [[pp00-l06]]

#swap #lecture06 #pab