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

module Liquidation
  ( liquidation
  , liquidationShortBs
  , validator
  , typedValidator
  , liquidationAddress
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
import           Ledger.Typed.Scripts as Scripts
import qualified Ledger as L
import qualified Common.Utils             as U
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import           Prelude              (Show (..))

data ContractInfo = ContractInfo
    { borrowerNftCs  :: !CurrencySymbol
    } deriving (Show, Generic, ToJSON, FromJSON)

{-# INLINABLE mkValidator #-}
mkValidator :: ContractInfo -> TokenName -> Integer -> ScriptContext -> Bool
mkValidator contractInfo@ContractInfo{..} borrowerNftTn _ ctx = case U.mintFlattened ctx of
      [(cs, tn, amt)] -> (amt == (-1)) &&
                         (tn == borrowerNftTn) &&
                         (cs == borrowerNftCs)
      _               -> False

data Liquidation
instance Scripts.ValidatorTypes Liquidation where
    type instance DatumType Liquidation = TokenName
    type instance RedeemerType Liquidation = Integer

typedValidator :: ContractInfo -> Scripts.TypedValidator Liquidation
typedValidator contractInfo = Scripts.mkTypedValidator @Liquidation
    ($$(PlutusTx.compile [|| mkValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode contractInfo)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @TokenName @Integer

validator :: ContractInfo -> Validator
validator = Scripts.validatorScript . typedValidator

script :: ContractInfo -> Plutus.Script
script = Plutus.unValidatorScript . validator

liquidationShortBs :: ContractInfo -> SBS.ShortByteString
liquidationShortBs = SBS.toShort . LBS.toStrict . serialise . script

liquidation :: ContractInfo -> PlutusScript PlutusScriptV1
liquidation = PlutusScriptSerialised . liquidationShortBs

liquidationAddress :: ContractInfo -> L.Address
liquidationAddress = L.scriptHashAddress . validatorHash . typedValidator

PlutusTx.makeIsDataIndexed ''ContractInfo [('ContractInfo, 1)]
PlutusTx.makeLift ''ContractInfo
