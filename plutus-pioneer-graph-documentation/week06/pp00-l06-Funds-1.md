
#### id: [[pp00-l06-Funds-1]]
## Contract: [`Funds.hs`](https://github.com/input-output-hk/plutus-pioneer-program/blob/main/code/week06/src/Week06/Oracle/Funds.hs)

---
This is the Plutus Pioneer first ever course. Thaught by [Dr. Lars Brünjes](https://github.com/brunjlar). Find this lecture on [Youtube](https://www.youtube.com/watch?v=wY7R-PJn66g&t=4865s) and [Github](https://github.com/input-output-hk/plutus-pioneer-program/tree/main/code/week06).
---

---
This is the third contract of Lecture 6. Refer to [[pp00-l06]]
---
Contains two simple functions
-   `ownFunds` sums up all the values in all UTXOs of one user (given his `pubKeyAddress`)
-   `ownFunds'` runs recursively in every slot. Uses Monadic bind to log the updated value of a `pubKeyAddress` 

## Full contract

```haskell
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Week06.Oracle.Funds
    ( ownFunds
    , ownFunds'
    ) where

import           Control.Monad    hiding (fmap)
import qualified Data.Map         as Map
import           Data.Monoid      (Last (..))
import           Data.Text        (Text)
import           Plutus.Contract  as Contract hiding (when)
import           PlutusTx.Prelude hiding ((<$>))
import           Prelude          ((<$>))
import           Ledger           hiding (singleton)
import           Ledger.Value     as Value

ownFunds :: HasBlockchainActions s => Contract w s Text Value
ownFunds = do
    pk    <- ownPubKey
    utxos <- utxoAt $ pubKeyAddress pk
    let v = mconcat $ Map.elems $ txOutValue . txOutTxOut <$> utxos
    logInfo @String $ "own funds: " ++ show (Value.flattenValue v)
    return v

ownFunds' :: Contract (Last Value) BlockchainActions Text ()
ownFunds' = do
    handleError logError $ ownFunds >>= tell . Last . Just
    void $ Contract.waitNSlots 1
    ownFunds'
```

---
2021-06-05 14:32
#### in [[pp00-l06]]

#plutus-pioneer  #lecture06 #funds