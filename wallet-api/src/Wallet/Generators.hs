{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
module Wallet.Generators(
    -- * Mockchain
    Mockchain(..),
    genMockchain,
    genMockchain',
    emptyChain,
    GeneratorModel(..),
    generatorModel,
    -- * Transactions
    FeeEstimator(..),
    constantFee,
    genValidTransaction,
    genValidTransaction',
    genValidTransactionSpending,
    genValidTransactionSpending',
    genInitialTransaction,
    -- * Assertions
    assertValid,
    -- * Etc.
    genAda,
    genValue,
    genValueNonNegative,
    Wallet.Generators.runTrace,
    runTraceOn,
    splitVal,
    validateMockchain
    ) where

import           Data.Bifunctor              (Bifunctor (..))
import           Data.Foldable               (foldl')
import           Data.Map                    (Map)
import qualified Data.Map                    as Map
import           Data.Maybe                  (catMaybes, isNothing)
import           Data.Set                    (Set)
import qualified Data.Set                    as Set
import           GHC.Stack                   (HasCallStack)
import           Hedgehog
import qualified Hedgehog.Gen                as Gen
import qualified Hedgehog.Range              as Range
import qualified Ledger.Interval             as Interval
import qualified Ledger.Index                as Index
import qualified Ledger.Value                as Value
import qualified Ledger.Ada                  as Ada

import           KeyBytes                    (fromHex)
import           Ledger
import qualified Wallet.API      as W
import           Wallet.Emulator as Emulator

data GeneratorModel = GeneratorModel {
    gmInitialBalance :: Map PubKey Value,
    -- ^ Value created at the beginning of the blockchain
    gmPubKeys        :: Set PubKey
    -- ^ Public keys that are to be used for generating transactions
    } deriving Show

-- | A generator model with some sensible defaults
generatorModel :: GeneratorModel
generatorModel = 
    let vl = Ada.toValue $ Ada.fromInt 100000
        pubKeys = fromHex <$> [ "d75a980182b10ab7d54bfed3c964073a0ee172f3daa62325af021a68f707511a"
                              , "3d4017c3e843895a92b70aa74d1b7ebc9c982ccf2ec4968cc0cd55f12af4660c"
                              , "fc51cd8e6218a1a38da47ed00230f0580816ed13ba3303ac5deb911548908025"
                              , "e61a185bcef2613a6c7cb79763ce945d3b245d76114dd440bcf5f2dc1aa57057"
                              , "c0dac102c4533186e25dc43128472353eaabdb878b152aeb8e001f92d90233a7"
                              ]
    in
    GeneratorModel 
    { gmInitialBalance = Map.fromList $ first PubKey <$> zip pubKeys (repeat vl)
    , gmPubKeys        = Set.fromList $ PubKey <$> pubKeys
    }

-- | Estimate a transaction fee based on the number of its inputs and outputs.
newtype FeeEstimator = FeeEstimator { estimateFee :: Int -> Int -> Ada }

-- | A constant fee for all transactions
constantFee :: Ada -> FeeEstimator
constantFee = FeeEstimator . const . const

-- | Blockchain for testing the emulator implementation and traces.
--
--   To avoid having to rely on functions from the implementation of
--   wallet-api (in particular, `Ledger.Types.unspentOutputs`) we note the
--   unspent outputs of the chain when it is first created.
data Mockchain = Mockchain {
    mockchainInitialBlock :: Block,
    mockchainUtxo         :: Map TxOutRef TxOut
    } deriving Show

-- | The empty mockchain
emptyChain :: Mockchain
emptyChain = Mockchain [] Map.empty

-- | Generate a mockchain
--
--   TODO: Generate more than 1 txn
genMockchain' :: MonadGen m
    => GeneratorModel
    -> m Mockchain
genMockchain' gm = do
    let (txn, ot) = genInitialTransaction gm
        txId = hashTx txn
    pure Mockchain {
        mockchainInitialBlock = [txn],
        mockchainUtxo = Map.fromList $ first (TxOutRefOf txId) <$> zip [0..] ot
        }

-- | Generate a mockchain using the default [[GeneratorModel]]
--
genMockchain :: MonadGen m => m Mockchain
genMockchain = genMockchain' generatorModel

-- | A transaction with no inputs that forges some value (to be used at the
--   beginning of a blockchain)
genInitialTransaction ::
       GeneratorModel
    -> (Tx, [TxOut])
genInitialTransaction GeneratorModel{..} =
    let
        o = (uncurry $ flip pubKeyTxOut) <$> Map.toList gmInitialBalance
        t = Map.foldl' Value.plus Value.zero gmInitialBalance
    in (Tx {
        txInputs = Set.empty,
        txOutputs = o,
        txForge = t,
        txFee = 0,
        txValidRange = W.intervalFrom 0,
        txSignatures = Map.empty
        }, o)

-- | Generate a valid transaction, using the unspent outputs provided.
--   Fails if the there are no unspent outputs, or if the total value
--   of the unspent outputs is smaller than the minimum fee (1)
genValidTransaction :: MonadGen m
    => Mockchain
    -> m Tx
genValidTransaction = genValidTransaction' generatorModel (constantFee 1)

-- | Generate a valid transaction, using the unspent outputs provided.
--   Fails if the there are no unspent outputs, or if the total value
--   of the unspent outputs is smaller than the estimated fee.
genValidTransaction' :: MonadGen m
    => GeneratorModel
    -> FeeEstimator
    -> Mockchain
    -> m Tx
genValidTransaction' g f (Mockchain bc ops) = do
    -- Take a random number of UTXO from the input
    nUtxo <- if Map.null ops
                then Gen.discard
                else Gen.int (Range.linear 1 (Map.size ops))
    let ins = Set.fromList
                    $ uncurry pubKeyTxIn
                    <$> (catMaybes
                        $ traverse (pubKeyTxo [bc]) . (di . fst) <$> inUTXO)
        inUTXO = take nUtxo $ Map.toList ops
        totalVal = foldl' (+) 0 $ (map (Ada.fromValue . txOutValue . snd) inUTXO)
        di a = (a, a)
    genValidTransactionSpending' g f ins totalVal

genValidTransactionSpending :: MonadGen m
    => Set.Set TxIn
    -> Ada
    -> m Tx
genValidTransactionSpending = genValidTransactionSpending' generatorModel (constantFee 1)

genValidTransactionSpending' :: MonadGen m
    => GeneratorModel
    -> FeeEstimator
    -> Set.Set TxIn
    -> Ada
    -> m Tx
genValidTransactionSpending' g f ins totalVal = do
    let fee = estimateFee f (length ins) 3
        numOut = Set.size $ gmPubKeys g
    if fee < totalVal
        then do
            let sz = totalVal - fee
            outVals <- fmap (Ada.toValue) <$> splitVal numOut sz
            pure Tx {
                    txInputs = ins,
                    txOutputs = uncurry pubKeyTxOut <$> zip outVals (Set.toList $ gmPubKeys g),
                    txForge = Value.zero,
                    txFee = fee,
                    txValidRange = $$(Interval.always),
                    txSignatures = Map.empty -- TODO: Insert signatures of the chosen keys
                    }
        else Gen.discard

genAda :: MonadGen m => m Ada
genAda = Ada.fromInt <$> Gen.int (Range.linear 0 (100000 :: Int))

genValue' :: MonadGen m => Range Int -> m Value
genValue' valueRange = do
    let currencyRange = Range.linearBounded @Int
        sngl          = Value.singleton <$> (Value.currencySymbol <$> Gen.int currencyRange) <*> Gen.int valueRange

        -- generate values with no more than 10 elements to avoid the tests 
        -- taking too long (due to the map-as-list-of-kv-pairs implementation)
        maxCurrencies = 10

    numValues <- Gen.int (Range.linear 0 maxCurrencies)
    values <- traverse (const sngl) [0 .. numValues]
    pure $ foldr Value.plus Value.zero values

-- | Generate a 'Value' with a coin value range of @minBound .. maxBound@
genValue :: MonadGen m => m Value
genValue = genValue' Range.linearBounded

-- | Generate a 'Value' with a coin value range of @0 .. maxBound@
genValueNonNegative :: MonadGen m => m Value
genValueNonNegative = genValue' (Range.linear 0 maxBound)
    
-- | Assert that a transaction is valid in a chain
assertValid :: (MonadTest m, HasCallStack)
    => Tx
    -> Mockchain
    -> m ()
assertValid tx mc = Hedgehog.assert $ isNothing $ validateMockchain mc tx

-- | Validate a transaction in a mockchain
validateMockchain :: Mockchain -> Tx -> Maybe Index.ValidationError
validateMockchain (Mockchain blck _) tx = either Just (const Nothing) result where
    h      = lastSlot [blck]
    idx    = Index.initialise [blck]
    result = Index.runValidation (Index.validateTransaction h tx) idx

-- | Run an emulator trace on a mockchain
runTrace ::
    Mockchain
    -> Trace MockWallet a
    -> (Either AssertionError a, EmulatorState)
runTrace (Mockchain ch _) = Emulator.runTraceTxPool ch

-- | Run an emulator trace on a mockchain generated by the model
runTraceOn :: MonadGen m
    => GeneratorModel
    -> Trace MockWallet a
    -> m (Either AssertionError a, EmulatorState)
runTraceOn gm t = flip Wallet.Generators.runTrace t <$> genMockchain' gm

-- | Split a value into max. n positive-valued parts such that the sum of the
--   parts equals the original value.
splitVal :: (MonadGen m, Integral n) => Int -> n -> m [n]
splitVal mx init' = go 0 0 [] where
    go i c l =
        if i >= pred mx
        then pure $ (init' - c) : l
        else do
            v <- Gen.integral (Range.linear 1 $ init' - c)
            if v + c == init'
            then pure $ v : l
            else go (succ i) (v + c) (v : l)
