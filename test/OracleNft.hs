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
mkPolicy :: BuiltinData -> ScriptContext -> Bool
mkPolicy _redeemer _ctx = True

policy :: Scripts.MintingPolicy
policy = mkMintingPolicyScript
    $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy mkPolicy ||])

plutusScript :: Script
plutusScript = unMintingPolicyScript policy

validator :: Validator
validator = Validator plutusScript

scriptAsCbor :: LB.ByteString
scriptAsCbor = serialise validator

oracleNft :: PlutusScript PlutusScriptV1
oracleNft = PlutusScriptSerialised . SBS.toShort $ LB.toStrict scriptAsCbor

oracleNftShortBs :: SBS.ShortByteString
oracleNftShortBs = SBS.toShort $ LB.toStrict scriptAsCbor
