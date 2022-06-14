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
  , OracleData(..)
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

data OracleData = OracleData {
    tradedCurrency :: CurrencySymbol
  , tokenName      :: TokenName
  , valInLovelaces :: Integer
} deriving (Show, Generic, FromJSON, ToJSON)

{-# INLINABLE mkPolicy #-}
mkPolicy :: TxOutRef -> TokenName -> PubKeyHash -> PubKeyHash -> PubKeyHash -> ValidatorHash -> OracleData -> ScriptContext -> Bool
mkPolicy utxo tn pkh1 pkh2 pkh3 dest _redeemer ctx = validate
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    hasUTxO :: Bool
    hasUTxO = any (\i -> txInInfoOutRef i == utxo) $ txInfoInputs info

    mintedValueSentToDest :: Bool
    mintedValueSentToDest = any (\x -> (toValidatorHash . txOutAddress) x == Just dest &&
                                        valueOf (txInfoMint info) (ownCurrencySymbol ctx) tn == 1 &&
                                        valueOf (txOutValue x) (ownCurrencySymbol ctx) tn == 1
                                        ) (txInfoOutputs info)
    burn :: Bool
    burn = valueOf (txInfoMint info) (ownCurrencySymbol ctx) tn < 0

    validate = hasUTxO && txSignedBy info pkh1 && txSignedBy info pkh2 && txSignedBy info pkh3 && mintedValueSentToDest || burn

policy :: TxOutRef -> TokenName -> PubKeyHash -> PubKeyHash -> PubKeyHash -> ValidatorHash -> Scripts.MintingPolicy
policy utxo tn pkh1 pkh2 pkh3 dest = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| wrap ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode utxo
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
    where
      wrap utxo' tn' pkh1' pkh2' pkh3' dest' = Scripts.wrapMintingPolicy $ mkPolicy utxo' tn' pkh1' pkh2' pkh3' dest' 

plutusScript :: TxOutRef -> TokenName -> PubKeyHash -> PubKeyHash -> PubKeyHash -> ValidatorHash -> Script
plutusScript utxo tn pkh1 pkh2 pkh3 dest  = unMintingPolicyScript $ policy utxo tn pkh1 pkh2 pkh3 dest

validator :: TxOutRef -> TokenName -> PubKeyHash -> PubKeyHash -> PubKeyHash -> ValidatorHash -> Validator
validator utxo tn pkh1 pkh2 pkh3 dest = Validator $ plutusScript utxo tn pkh1 pkh2 pkh3 dest

scriptAsCbor :: TxOutRef -> TokenName -> PubKeyHash -> PubKeyHash -> PubKeyHash -> ValidatorHash -> LB.ByteString
scriptAsCbor utxo tn pkh1 pkh2 pkh3 dest  = serialise $ validator utxo tn pkh1 pkh2 pkh3 dest

oracleNft :: TxOutRef -> TokenName -> PubKeyHash -> PubKeyHash -> PubKeyHash -> ValidatorHash -> PlutusScript PlutusScriptV1
oracleNft utxo tn pkh1 pkh2 pkh3 dest  = PlutusScriptSerialised $ SBS.toShort $ LB.toStrict $ scriptAsCbor utxo tn pkh1 pkh2 pkh3 dest

oracleNftShortBs :: TxOutRef -> TokenName -> PubKeyHash -> PubKeyHash -> PubKeyHash -> ValidatorHash -> SBS.ShortByteString
oracleNftShortBs utxo tn pkh1 pkh2 pkh3 dest = SBS.toShort $ LB.toStrict $ scriptAsCbor utxo tn pkh1 pkh2 pkh3 dest

PlutusTx.makeLift ''OracleData
PlutusTx.makeIsDataIndexed ''OracleData [('OracleData, 0)]
