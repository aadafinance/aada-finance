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

module Helpers.TestValidator
  ( testValidator
  , testValidatorShortBs
  , validator
  , typedValidator
  , mkValidator
  , failValidator
  ) where

import           Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV1)

import           Codec.Serialise
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS

import           Plutus.V1.Ledger.Contexts
import qualified Plutus.V1.Ledger.Scripts as Plutus
import           Plutus.V1.Ledger.Scripts
import qualified PlutusTx
import           PlutusTx.Prelude hiding (Semigroup (..), unless)

import           Ledger.Typed.Scripts as Scripts

{-# INLINABLE mkValidator #-}
mkValidator :: Integer -> Integer -> ScriptContext -> Bool
mkValidator lock guess _ = lock == guess

{-# INLINABLE alwaysFail #-}
alwaysFail :: Integer -> Integer -> ScriptContext -> Bool
alwaysFail _ _ _ = False

data TestValidator
instance Scripts.ValidatorTypes TestValidator where
    type instance DatumType TestValidator = Integer
    type instance RedeemerType TestValidator = Integer

typedValidator :: Scripts.TypedValidator TestValidator
typedValidator = Scripts.mkTypedValidator @TestValidator
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @Integer @Integer

data FailValidator
instance Scripts.ValidatorTypes FailValidator where
    type instance DatumType FailValidator = Integer
    type instance RedeemerType FailValidator = Integer

failValidator :: Scripts.TypedValidator FailValidator
failValidator = Scripts.mkTypedValidator @FailValidator
    $$(PlutusTx.compile [|| alwaysFail ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @Integer @Integer

validator :: Validator
validator = Scripts.validatorScript typedValidator

script :: Plutus.Script
script = Plutus.unValidatorScript validator

testValidatorShortBs :: SBS.ShortByteString
testValidatorShortBs = SBS.toShort . LBS.toStrict $ serialise script

testValidator :: PlutusScript PlutusScriptV1
testValidator = PlutusScriptSerialised testValidatorShortBs
