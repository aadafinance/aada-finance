{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module OracleNft
  ( oracleNft
  , oracleCs
  ) where

import           Codec.Serialise
import qualified Data.ByteString.Lazy     as LB
import qualified Data.ByteString.Short    as SBS
import qualified PlutusTx
import           PlutusTx.Prelude         hiding (Semigroup (..), unless)
import qualified Plutus.V1.Ledger.Scripts as Plutus
import           Plutus.V2.Ledger.Contexts
import           Plutus.V1.Ledger.Scripts
import           Plutus.V1.Ledger.Value
import           Plutus.V2.Ledger.Api
import           Plutus.V1.Ledger.Address
import           Prelude                  (Semigroup (..), Show)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import qualified Common.Utils             as U
import           Plutus.Model.V1

{-# INLINABLE mkPolicy #-}
mkPolicy
    :: TokenName
    -> PubKeyHash
    -> PubKeyHash
    -> PubKeyHash
    -> BuiltinByteString
    -> BuiltinData
    -> BuiltinData
    -> ()
mkPolicy tn pkh1 pkh2 pkh3 dest _redeemer (unsafeFromBuiltinData -> ctx :: ScriptContext) = check validate
  where
    checkTargetAddress :: Address -> Bool
    checkTargetAddress addr = case toValidatorHash addr of
      Just hash -> hash == ValidatorHash dest
      Nothing   -> case toPubKeyHash addr of
        Just pkh -> pkh == PubKeyHash dest
        Nothing  -> False

    mintedValueSentToDest :: Bool
    mintedValueSentToDest = any
      (\x -> checkTargetAddress (txOutAddress x) &&
       valueOf (txInfoMint (U.info ctx)) (ownCurrencySymbol ctx) tn == 1 &&
       valueOf (txOutValue x) (ownCurrencySymbol ctx) tn == 1)
      (txInfoOutputs (U.info ctx))

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
      txSignedBy (U.info ctx) pkh1 &&
      txSignedBy (U.info ctx) pkh2 &&
      txSignedBy (U.info ctx) pkh3 &&
      mintedValueSentToDest || burn

oracleNft :: TokenName -> PubKeyHash -> PubKeyHash -> PubKeyHash -> BuiltinByteString -> Script
oracleNft tn pkh1 pkh2 pkh3 dest = fromCompiledCode $
    $$(PlutusTx.compile [|| mkPolicy ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode tn
    `PlutusTx.applyCode`
    PlutusTx.liftCode pkh1
    `PlutusTx.applyCode`
    PlutusTx.liftCode pkh2
    `PlutusTx.applyCode`
    PlutusTx.liftCode pkh3
    `PlutusTx.applyCode`
    PlutusTx.liftCode dest

-- part below is for plutus-simple-model testing
typedPolicy :: TokenName -> PubKeyHash -> PubKeyHash -> PubKeyHash -> BuiltinByteString -> TypedPolicy BuiltinData
typedPolicy tn pkh1 pkh2 pkh3 dest = mkTypedPolicy $
    $$(PlutusTx.compile [|| mkPolicy ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode tn
    `PlutusTx.applyCode`
    PlutusTx.liftCode pkh1
    `PlutusTx.applyCode`
    PlutusTx.liftCode pkh2
    `PlutusTx.applyCode`
    PlutusTx.liftCode pkh3
    `PlutusTx.applyCode`
    PlutusTx.liftCode dest

oracleCs :: TokenName -> PubKeyHash -> PubKeyHash -> PubKeyHash -> BuiltinByteString -> CurrencySymbol
oracleCs tn pkh1 pkh2 pkh3 dest = scriptCurrencySymbol $ typedPolicy tn pkh1 pkh2 pkh3 dest
