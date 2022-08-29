{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module NFTStaking
  ( nftstaking
  , nftstakingShortBs
  ) where

import           Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV1)

import           Codec.Serialise
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS

import           Plutus.V1.Ledger.Contexts
import qualified Plutus.V1.Ledger.Scripts as Plutus
import           Plutus.V1.Ledger.Scripts
import           Plutus.V1.Ledger.Address
import           Plutus.V1.Ledger.Crypto
import           Plutus.V1.Ledger.Value
import qualified PlutusTx
import           PlutusTx.Prelude hiding (Semigroup (..), unless)
import           Prelude              (IO, Semigroup (..), Show (..), String)

import           Ledger.Typed.Scripts as Scripts


data NFTStakingDatum = NFTStakingDatum
    { lockedTokensID    :: !CurrencySymbol
    , lockedTokenName   :: !TokenName
    } deriving Show

{-# INLINABLE mkValidator #-}
mkValidator :: NFTStakingDatum -> Integer -> ScriptContext -> Bool
mkValidator dat _ ctx = traceIfFalse "Validation failed" validate
  where
    txOuts :: [TxOut]
    txOuts = txInfoOutputs info

    info :: TxInfo
    info = scriptContextTxInfo ctx

    validateBurn :: Bool
    validateBurn = case flattenValue (txInfoMint info) of
      [(cs, tn, amt)] -> cs == (lockedTokensID dat) && tn == (lockedTokenName dat) && amt == (-2)
      _               -> traceIfFalse "Validate burn is false. No burn." False

    getOutputPkh :: Maybe PubKeyHash
    getOutputPkh = case nub $ toPubKeyHash . txOutAddress <$> txOuts of
      [pkh] -> pkh
      _     -> Nothing

    getInputHashes :: [Maybe PubKeyHash]
    getInputHashes = toPubKeyHash . txOutAddress . txInInfoResolved <$> txInfoInputs info

    elem' :: Eq a => (Maybe a) -> [a] -> Bool
    elem' Nothing  _  = False
    elem' (Just x) xs = elem x xs

    validateOutput :: Bool
    validateOutput = elem' getOutputPkh [hashes | Just hashes <- getInputHashes]

    tokenFilter :: TxOut -> Bool
    tokenFilter tx = valueOf (txOutValue tx) (lockedTokensID dat) (lockedTokenName dat) > 0

    getHashesWithTokens :: [Maybe PubKeyHash]
    getHashesWithTokens = fmap (toPubKeyHash . txOutAddress) $ filter tokenFilter $ txInInfoResolved <$> txInfoInputs info

    validateInput :: Bool
    validateInput = elem getOutputPkh getHashesWithTokens

    validate :: Bool
    validate = traceIfFalse "No input tx with tokens found" validateInput &&
               traceIfFalse "Not everything going to single pkh" validateOutput &&
               traceIfFalse "Tokens not burnedd" validateBurn

data NFTStaking
instance Scripts.ValidatorTypes NFTStaking where
    type instance DatumType NFTStaking = NFTStakingDatum
    type instance RedeemerType NFTStaking = Integer

typedValidator :: Scripts.TypedValidator NFTStaking
typedValidator = Scripts.mkTypedValidator @NFTStaking
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @NFTStakingDatum @Integer

validator :: Validator
validator = Scripts.validatorScript typedValidator

PlutusTx.makeIsDataIndexed ''NFTStakingDatum [('NFTStakingDatum, 0)]

script :: Plutus.Script
script = Plutus.unValidatorScript validator

nftstakingShortBs :: SBS.ShortByteString
nftstakingShortBs = SBS.toShort . LBS.toStrict $ serialise script

nftstaking :: PlutusScript PlutusScriptV1
nftstaking = PlutusScriptSerialised nftstakingShortBs