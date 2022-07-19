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
  , OracleData(..)
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

data OracleData = OracleData {
    collateralRatio :: Integer
  , loanValueInAda  :: Integer
  , collateralVal   :: Integer
  , interestVal     :: Integer
  , receivVal       :: Integer
  , sendVal         :: Integer
} deriving (Show, Generic, FromJSON, ToJSON)

{-# INLINABLE mkPolicy #-}
mkPolicy :: TokenName -> PubKeyHash -> PubKeyHash -> PubKeyHash -> BuiltinByteString -> BuiltinData -> BuiltinData -> ()
mkPolicy tn pkh1 pkh2 pkh3 dest _redeemer (unsafeFromBuiltinData -> ctx :: ScriptContext) = check validate
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    checkTargetAddress :: Address -> Bool
    checkTargetAddress addr = case toValidatorHash addr of
      Just hash -> hash == (ValidatorHash dest)
      Nothing   -> case toPubKeyHash addr of 
        Just pkh -> pkh == (PubKeyHash dest)
        Nothing  -> False

    mintedValueSentToDest :: Bool
    mintedValueSentToDest = any (\x -> (checkTargetAddress (txOutAddress x)) &&
                                        valueOf (txInfoMint info) (ownCurrencySymbol ctx) tn == 1 &&
                                        valueOf (txOutValue x) (ownCurrencySymbol ctx) tn == 1
                                        ) (txInfoOutputs info)
    burn :: Bool
    burn = valueOf (txInfoMint info) (ownCurrencySymbol ctx) tn < 0

    validate = traceIfFalse "oracle nft wasn't signed by pkh1" (txSignedBy info pkh1) &&
               traceIfFalse "oracle nft wasn't signed by pkh2" (txSignedBy info pkh2) &&
               traceIfFalse "oracle nft wasn't signed by pkh3" (txSignedBy info pkh3) &&
               traceIfFalse "minted oracle nft not sent to validator hash specified in minting policy" mintedValueSentToDest || burn

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
