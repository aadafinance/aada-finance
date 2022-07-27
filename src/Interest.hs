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

module Interest
  ( interest
  , interestShortBs
  , validator
  , typedValidator
  , interestAddress
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

{-# INLINABLE flattenBuiltinByteString #-}
flattenBuiltinByteString :: [BuiltinByteString] -> BuiltinByteString
flattenBuiltinByteString [] = emptyByteString
flattenBuiltinByteString (x:xs) = appendByteString x $ flattenBuiltinByteString xs

{-# INLINABLE lender #-}
lender :: TokenName
lender = TokenName { unTokenName = flattenBuiltinByteString [consByteString x emptyByteString | x <- [76]]}  -- L

{-# INLINABLE mkValidator #-}
mkValidator :: Integer -> Integer -> ScriptContext -> Bool
mkValidator _ _ ctx = validate
  where
    ownInput :: Maybe TxOut
    ownInput = case findOwnInput ctx of
      Just txin -> Just $ txInInfoResolved txin
      Nothing   -> Nothing

    hasBurntNft :: CurrencySymbol -> Bool
    hasBurntNft cs = case ownInput of
      Just txo -> valueOf (txOutValue txo) cs lender == 1
      Nothing  -> False

    validate :: Bool
    validate = case U.mintFlattened ctx of
      [(cs, tn, amt)] -> (amt == (-2)) &&
                         hasBurntNft cs &&
                         (tn == lender)
      _               -> False

data Interest
instance Scripts.ValidatorTypes Interest where
    type instance DatumType Interest = Integer
    type instance RedeemerType Interest = Integer

typedValidator :: Scripts.TypedValidator Interest
typedValidator = Scripts.mkTypedValidator @Interest
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @Integer @Integer

validator :: Validator
validator = Scripts.validatorScript typedValidator

script :: Plutus.Script
script = Plutus.unValidatorScript validator

interestShortBs :: SBS.ShortByteString
interestShortBs = SBS.toShort . LBS.toStrict $ serialise script

interest :: PlutusScript PlutusScriptV1
interest = PlutusScriptSerialised interestShortBs

interestAddress :: L.Address
interestAddress = L.scriptHashAddress $ validatorHash typedValidator