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
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}

module AadaNft
  ( aadaNft
  , aadaNftShortBs
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
import qualified Common.Utils             as U

{-# INLINABLE mkPolicy #-}
mkPolicy :: Bool -> TxOutRef -> ScriptContext -> Bool
mkPolicy isLender utxo ctx = case mintedValue of
    [(_cs, tn, n)] -> validateMint tn n
    _     -> False
  where
    mintFlattened :: [(CurrencySymbol, TokenName, Integer)]
    mintFlattened = flattenValue $ txInfoMint (scriptContextTxInfo ctx)

    mintedValue :: [(CurrencySymbol, TokenName, Integer)]
    mintedValue = filter (\(cs, _tn, _n) -> cs == ownCurrencySymbol ctx) mintFlattened

    calculateTokenNameHash :: BuiltinByteString
    calculateTokenNameHash = sha2_256 (consByteString (txOutRefIdx utxo) ((getTxId . txOutRefId) utxo)) 

    validateTokenName :: TokenName -> Bool
    validateTokenName tn = unTokenName tn == calculateTokenNameHash

    checkForOverflow :: Bool
    checkForOverflow = txOutRefIdx utxo < 256

    validateMint :: TokenName -> Integer -> Bool
    validateMint tn amount = U.hasUTxO utxo ctx &&
                             traceIfFalse "invalid lender nft minted amount" (amount == 1) &&
                             traceIfFalse "minted nft has invalid token name" (validateTokenName tn) &&
                             traceIfFalse "txOutRefIdx of provided utxo is too big " checkForOverflow ||
                             traceIfFalse "invalid burn amount" (amount == (-1))

policy :: Bool -> Scripts.MintingPolicy
policy isLender = mkMintingPolicyScript $
   $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy . mkPolicy ||])
   `PlutusTx.applyCode`
   PlutusTx.liftCode isLender

plutusScript :: Bool -> Script
plutusScript = unMintingPolicyScript . policy

validator :: Bool -> Validator
validator = Validator . plutusScript

scriptAsCbor :: Bool -> LB.ByteString
scriptAsCbor = serialise . validator

aadaNft :: Bool -> PlutusScript PlutusScriptV1
aadaNft = PlutusScriptSerialised . SBS.toShort . LB.toStrict . scriptAsCbor

aadaNftShortBs :: Bool -> SBS.ShortByteString
aadaNftShortBs = SBS.toShort . LB.toStrict . scriptAsCbor
