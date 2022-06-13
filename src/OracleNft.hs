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

{-# INLINABLE mkPolicy #-}
mkPolicy :: PubKeyHash -> PubKeyHash -> PubKeyHash -> ValidatorHash -> BuiltinData -> ScriptContext -> Bool
mkPolicy pkh1 pkh2 pkh3 dest _redeemer ctx = validate
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx
    
    validate = txSignedBy info pkh1 && txSignedBy info pkh2 && txSignedBy info pkh3

policy :: PubKeyHash -> PubKeyHash -> PubKeyHash -> ValidatorHash -> Scripts.MintingPolicy
policy pkh1 pkh2 pkh3 dest = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \pkh1' pkh2' pkh3' dest' -> Scripts.wrapMintingPolicy $ mkPolicy pkh1' pkh2' pkh3' dest' ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode pkh1
    `PlutusTx.applyCode`
    PlutusTx.liftCode pkh2
    `PlutusTx.applyCode`
    PlutusTx.liftCode pkh3
    `PlutusTx.applyCode`
    PlutusTx.liftCode dest

plutusScript :: PubKeyHash -> PubKeyHash -> PubKeyHash -> ValidatorHash -> Script
plutusScript pkh1 pkh2 pkh3 dest  = unMintingPolicyScript $ policy pkh1 pkh2 pkh3 dest

validator :: PubKeyHash -> PubKeyHash -> PubKeyHash -> ValidatorHash -> Validator
validator pkh1 pkh2 pkh3 dest = Validator $ plutusScript pkh1 pkh2 pkh3 dest

scriptAsCbor :: PubKeyHash -> PubKeyHash -> PubKeyHash -> ValidatorHash -> LB.ByteString
scriptAsCbor pkh1 pkh2 pkh3 dest  = serialise $ validator pkh1 pkh2 pkh3 dest

oracleNft :: PubKeyHash -> PubKeyHash -> PubKeyHash -> ValidatorHash -> PlutusScript PlutusScriptV1
oracleNft pkh1 pkh2 pkh3 dest  = PlutusScriptSerialised $ SBS.toShort $ LB.toStrict $ scriptAsCbor pkh1 pkh2 pkh3 dest

oracleNftShortBs :: PubKeyHash -> PubKeyHash -> PubKeyHash -> ValidatorHash -> SBS.ShortByteString
oracleNftShortBs pkh1 pkh2 pkh3 dest = SBS.toShort $ LB.toStrict $ scriptAsCbor pkh1 pkh2 pkh3 dest
