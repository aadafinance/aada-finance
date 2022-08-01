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

module LenderNft
  ( lenderNft
  , lenderNftShortBs
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
mkPolicy :: TxOutRef -> ScriptContext -> Bool
mkPolicy utxo ctx = case mintedValue of
    [val] -> validate val
    _     -> False
  where
    mintFlattened :: [(CurrencySymbol, TokenName, Integer)]
    mintFlattened = flattenValue $ txInfoMint (scriptContextTxInfo ctx)

    mintedValue :: [(CurrencySymbol, TokenName, Integer)]
    mintedValue = filter (\(cs, _tn, _n) -> cs == ownCurrencySymbol ctx) mintFlattened

    calculateTokenNameHash :: BuiltinByteString
    calculateTokenNameHash = consByteString (txOutRefIdx utxo) ((getTxId . txOutRefId) utxo)

    validateTokenName :: TokenName -> Bool
    validateTokenName tn = unTokenName tn == calculateTokenNameHash

    checkForOverflow :: Bool
    checkForOverflow = txOutRefIdx utxo < 256

    validateMint :: TokenName -> Integer -> Bool
    validateMint tn amount = U.hasUTxO utxo ctx &&
                             traceIfFalse "invalid lender nft minted amount" (amount == 2) &&
                             traceIfFalse "minted nft has invalid token name" (validateTokenName tn) &&
                             traceIfFalse "txOutRefId of provided utxo is too big " checkForOverflow

    validate :: (CurrencySymbol, TokenName, Integer) -> Bool
    validate (_cs, tn, n)
     | n > 0     = validateMint tn n
     | otherwise = True

policy :: Scripts.MintingPolicy
policy = mkMintingPolicyScript $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy mkPolicy ||])

plutusScript :: Script
plutusScript = unMintingPolicyScript policy

validator :: Validator
validator = Validator plutusScript

scriptAsCbor :: LB.ByteString
scriptAsCbor = serialise validator

lenderNft :: PlutusScript PlutusScriptV1
lenderNft = PlutusScriptSerialised . SBS.toShort . LB.toStrict $ scriptAsCbor

lenderNftShortBs :: SBS.ShortByteString
lenderNftShortBs = SBS.toShort . LB.toStrict $ scriptAsCbor
