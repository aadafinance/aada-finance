{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Interest
  ( interest
  , interestShortBs
  , validator
  , typedValidator
  , interestAddress
  , ContractInfo(..)
  ) where

import           Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV1)

import           Codec.Serialise
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS

import           Plutus.V1.Ledger.Contexts
import qualified Plutus.V1.Ledger.Scripts as Plutus
import           Plutus.V1.Ledger.Scripts
import           Plutus.V1.Ledger.Value
import qualified PlutusTx
import           PlutusTx.Prelude hiding (Semigroup (..), unless)
import           Prelude              (Show (..))
import           Ledger.Typed.Scripts as Scripts
import qualified Ledger as L
import qualified Common.Utils             as U
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

data ContractInfo = ContractInfo
    { lenderNftCs  :: !CurrencySymbol
    } deriving (Show, Generic, ToJSON, FromJSON)

{-# INLINABLE mkValidator #-}
mkValidator :: ContractInfo -> Integer -> Integer -> ScriptContext -> Bool
mkValidator contractInfo@ContractInfo{..} _ _ ctx = validate
  where
    hasBurntNft :: CurrencySymbol -> TokenName -> Bool
    hasBurntNft cs tn = case U.ownInput ctx of
      Just txo -> valueOf (txOutValue txo) cs tn == 1
      Nothing  -> False

    validate :: Bool
    validate = case U.mintFlattened ctx of
      [(cs, tn, amt)] -> (amt == (-2)) &&
                         cs == lenderNftCs &&
                         hasBurntNft cs tn
      _               -> False

data Interest
instance Scripts.ValidatorTypes Interest where
    type instance DatumType Interest = Integer
    type instance RedeemerType Interest = Integer

typedValidator :: ContractInfo -> Scripts.TypedValidator Interest
typedValidator contractInfo = Scripts.mkTypedValidator @Interest
    ($$(PlutusTx.compile [|| mkValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode contractInfo)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @Integer @Integer

validator :: ContractInfo -> Validator
validator = Scripts.validatorScript . typedValidator

script :: ContractInfo -> Plutus.Script
script = Plutus.unValidatorScript . validator

interestShortBs :: ContractInfo -> SBS.ShortByteString
interestShortBs = SBS.toShort . LBS.toStrict . serialise . script

interest :: ContractInfo -> PlutusScript PlutusScriptV1
interest = PlutusScriptSerialised . interestShortBs

interestAddress :: ContractInfo -> L.Address
interestAddress = L.scriptHashAddress . validatorHash . typedValidator

PlutusTx.makeIsDataIndexed ''ContractInfo [('ContractInfo, 1)]
PlutusTx.makeLift ''ContractInfo
