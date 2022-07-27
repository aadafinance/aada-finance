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
import qualified Common.Utils             as U

data OracleData = OracleData {
    collateralRatio :: Integer
  , loanValueInAda  :: Integer
  , collateralVal   :: Integer
  , interestVal     :: Integer
  , receivVal       :: Integer
  , sendVal         :: Integer
} deriving (Show, Generic, FromJSON, ToJSON)

{-# INLINABLE mkPolicy #-}
mkPolicy :: TokenName -> PubKeyHash -> PubKeyHash -> PubKeyHash -> BuiltinByteString -> OracleData -> ScriptContext -> Bool
mkPolicy tn pkh1 pkh2 pkh3 dest _redeemer ctx = validate
  where
    checkTargetAddress :: Address -> Bool
    checkTargetAddress addr = case toValidatorHash addr of
      Just hash -> hash == ValidatorHash dest
      Nothing   -> case toPubKeyHash addr of
        Just pkh -> pkh == PubKeyHash dest
        Nothing  -> False

    mintedValueSentToDest :: Bool
    mintedValueSentToDest = any (\x -> checkTargetAddress (txOutAddress x) &&
                                        valueOf (txInfoMint (U.info ctx)) (ownCurrencySymbol ctx) tn == 1 &&
                                        valueOf (txOutValue x) (ownCurrencySymbol ctx) tn == 1
                                        ) (txInfoOutputs (U.info ctx))
    burn :: Bool
    burn = valueOf (txInfoMint (U.info ctx)) (ownCurrencySymbol ctx) tn < 0

    validate = traceIfFalse "oracle nft wasn't signed by pkh1" (txSignedBy (U.info ctx) pkh1) &&
               traceIfFalse "oracle nft wasn't signed by pkh2" (txSignedBy (U.info ctx) pkh2) &&
               traceIfFalse "oracle nft wasn't signed by pkh3" (txSignedBy (U.info ctx) pkh3) &&
               traceIfFalse "minted oracle nft not sent to validator hash specified in minting policy" mintedValueSentToDest || burn

policy :: TokenName -> PubKeyHash -> PubKeyHash -> PubKeyHash -> BuiltinByteString -> Scripts.MintingPolicy
policy tn pkh1 pkh2 pkh3 dest = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| wrap ||])
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
      wrap tn' pkh1' pkh2' pkh3' dest' = Scripts.wrapMintingPolicy $ mkPolicy tn' pkh1' pkh2' pkh3' dest'

plutusScript :: TokenName -> PubKeyHash -> PubKeyHash -> PubKeyHash -> BuiltinByteString -> Script
plutusScript tn pkh1 pkh2 pkh3 dest  = unMintingPolicyScript $ policy tn pkh1 pkh2 pkh3 dest

validator :: TokenName -> PubKeyHash -> PubKeyHash -> PubKeyHash -> BuiltinByteString -> Validator
validator tn pkh1 pkh2 pkh3 dest = Validator $ plutusScript tn pkh1 pkh2 pkh3 dest

scriptAsCbor :: TokenName -> PubKeyHash -> PubKeyHash -> PubKeyHash -> BuiltinByteString -> LB.ByteString
scriptAsCbor tn pkh1 pkh2 pkh3 dest  = serialise $ validator tn pkh1 pkh2 pkh3 dest

oracleNft :: TokenName -> PubKeyHash -> PubKeyHash -> PubKeyHash -> BuiltinByteString -> PlutusScript PlutusScriptV1
oracleNft tn pkh1 pkh2 pkh3 dest  = PlutusScriptSerialised $ SBS.toShort $ LB.toStrict $ scriptAsCbor tn pkh1 pkh2 pkh3 dest

oracleNftShortBs :: TokenName -> PubKeyHash -> PubKeyHash -> PubKeyHash -> BuiltinByteString -> SBS.ShortByteString
oracleNftShortBs tn pkh1 pkh2 pkh3 dest = SBS.toShort $ LB.toStrict $ scriptAsCbor tn pkh1 pkh2 pkh3 dest

PlutusTx.makeLift ''OracleData
PlutusTx.makeIsDataIndexed ''OracleData [('OracleData, 0)]
