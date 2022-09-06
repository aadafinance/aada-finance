{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module OracleNft
  ( oracleNft
  , oracleNftShortBs
  , policy
  ) where

import           Cardano.Api.Shelley      (PlutusScript (..), PlutusScriptV1)
import           Codec.Serialise
import qualified Data.ByteString.Lazy     as LB
import qualified Data.ByteString.Short    as SBS
import           Ledger                   hiding (singleton)
import qualified Ledger.Typed.Scripts     as Scripts
import           Ledger.Value             as Value
import qualified PlutusTx
import           PlutusTx.Prelude         hiding (Semigroup (..), unless)
import qualified Plutus.V1.Ledger.Scripts as Plutus
import           Prelude                  (Semigroup (..), Show)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import qualified Common.Utils             as U

{-# INLINABLE mkPolicy #-}
mkPolicy
    :: TokenName
    -> PubKeyHash
    -> PubKeyHash
    -> PubKeyHash
    -> BuiltinData
    -> ScriptContext
    -> Bool
mkPolicy tn pkh1 pkh2 pkh3 _redeemer ctx = validate
  where
    mintFlattened :: [(CurrencySymbol, TokenName, Integer)]
    mintFlattened = flattenValue $ txInfoMint (scriptContextTxInfo ctx)

    ownMintedValue :: [(CurrencySymbol, TokenName, Integer)]
    ownMintedValue = filter (\(cs, _tn, _n) -> cs == ownCurrencySymbol ctx) mintFlattened

    singleTokenName :: Bool
    singleTokenName = all (\(_cs, tn', _n) -> tn == tn') ownMintedValue

    burn :: Bool
    burn = valueOf (txInfoMint (U.info ctx)) (ownCurrencySymbol ctx) tn < 0

    validate =
      singleTokenName &&
      (txSignedBy (U.info ctx) pkh1 &&
      txSignedBy (U.info ctx) pkh2 &&
      txSignedBy (U.info ctx) pkh3 || burn)

policy
    :: TokenName
    -> PubKeyHash
    -> PubKeyHash
    -> PubKeyHash
    -> Scripts.MintingPolicy
policy tn pkh1 pkh2 pkh3 = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| wrap ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode tn
    `PlutusTx.applyCode`
    PlutusTx.liftCode pkh1
    `PlutusTx.applyCode`
    PlutusTx.liftCode pkh2
    `PlutusTx.applyCode`
    PlutusTx.liftCode pkh3
  where
    wrap tn' pkh1' pkh2' pkh3' =
      Scripts.wrapMintingPolicy $ mkPolicy tn' pkh1' pkh2' pkh3'

plutusScript
    :: TokenName
    -> PubKeyHash
    -> PubKeyHash
    -> PubKeyHash
    -> Script
plutusScript tn pkh1 pkh2 pkh3 = unMintingPolicyScript $ policy tn pkh1 pkh2 pkh3

validator
  :: TokenName
  -> PubKeyHash
  -> PubKeyHash
  -> PubKeyHash
  -> Validator
validator tn pkh1 pkh2 pkh3 = Validator $ plutusScript tn pkh1 pkh2 pkh3

scriptAsCbor
    :: TokenName
    -> PubKeyHash
    -> PubKeyHash
    -> PubKeyHash
    -> LB.ByteString
scriptAsCbor tn pkh1 pkh2 pkh3 = serialise $ validator tn pkh1 pkh2 pkh3

oracleNft
    :: TokenName
    -> PubKeyHash
    -> PubKeyHash
    -> PubKeyHash
    -> PlutusScript PlutusScriptV1
oracleNft tn pkh1 pkh2 pkh3 =
  PlutusScriptSerialised $ SBS.toShort $ LB.toStrict $ scriptAsCbor tn pkh1 pkh2 pkh3

oracleNftShortBs
    :: TokenName
    -> PubKeyHash
    -> PubKeyHash
    -> PubKeyHash
    -> SBS.ShortByteString
oracleNftShortBs tn pkh1 pkh2 pkh3 =
  SBS.toShort $ LB.toStrict $ scriptAsCbor tn pkh1 pkh2 pkh3
