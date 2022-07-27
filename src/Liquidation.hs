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
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}

module Liquidation
  ( liquidation
  , liquidationShortBs
  , validator
  , typedValidator
  , liquidationAddress
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

{-# INLINABLE borrower #-}
borrower :: TokenName
borrower = TokenName { unTokenName = consByteString 66 emptyByteString }  -- B

{-# INLINABLE mkValidator #-}
mkValidator :: CurrencySymbol -> Integer -> ScriptContext -> Bool
mkValidator dat _ ctx = case U.mintFlattened ctx of
      [(cs, tn, amt)] -> (amt == (-1)) &&
                         (tn == borrower) &&
                         (cs == dat)
      _               -> False

data Liquidation
instance Scripts.ValidatorTypes Liquidation where
    type instance DatumType Liquidation = CurrencySymbol
    type instance RedeemerType Liquidation = Integer

typedValidator :: Scripts.TypedValidator Liquidation
typedValidator = Scripts.mkTypedValidator @Liquidation
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @CurrencySymbol @Integer

validator :: Validator
validator = Scripts.validatorScript typedValidator

script :: Plutus.Script
script = Plutus.unValidatorScript validator

liquidationShortBs :: SBS.ShortByteString
liquidationShortBs = SBS.toShort . LBS.toStrict $ serialise script

liquidation :: PlutusScript PlutusScriptV1
liquidation = PlutusScriptSerialised liquidationShortBs

liquidationAddress :: L.Address
liquidationAddress = L.scriptHashAddress $ validatorHash typedValidator
