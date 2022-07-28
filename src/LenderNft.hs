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
mkPolicy :: ValidatorHash -> TxOutRef -> ScriptContext -> Bool
mkPolicy vh utxo ctx = all validate mintedValue
  where
    mintedValue :: [(CurrencySymbol, TokenName, Integer)]
    mintedValue = filter (\(cs, _tn, _n) -> cs == ownCurrencySymbol ctx) $ U.mintFlattened ctx

    calculateTokenNameHash :: BuiltinByteString
    calculateTokenNameHash = sha2_256 (consByteString (txOutRefIdx utxo) ((getTxId . txOutRefId) utxo))

    validateTokenName :: TokenName -> Bool
    validateTokenName tn = unTokenName tn == calculateTokenNameHash

    nftIsSentToCollateralSc :: TokenName -> Bool
    nftIsSentToCollateralSc tn = traceIfFalse "minted lender nft is not sent to collateral smart contract" (valueOf (U.valueToSc vh ctx) (ownCurrencySymbol ctx) tn == 1)

    validateMint :: TokenName -> Integer -> Bool
    validateMint tn amount = traceIfFalse "invalid lender nft minted amount" (amount == 2) &&
                             traceIfFalse "Minted nft is not sent to collateral sc" (nftIsSentToCollateralSc tn) &&
                             traceIfFalse "minted nft has invalid token name" (validateTokenName tn)

    validate :: (CurrencySymbol, TokenName, Integer) -> Bool
    validate (_cs, tn, n)
     | n > 0     = validateMint tn n
     | otherwise = True

policy :: ValidatorHash -> Scripts.MintingPolicy
policy vh = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy . mkPolicy ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode vh

plutusScript :: ValidatorHash -> Script
plutusScript = unMintingPolicyScript . policy

validator :: ValidatorHash -> Validator
validator = Validator . plutusScript

scriptAsCbor :: ValidatorHash -> LB.ByteString
scriptAsCbor = serialise . validator

lenderNft :: ValidatorHash -> PlutusScript PlutusScriptV1
lenderNft = PlutusScriptSerialised . SBS.toShort . LB.toStrict . scriptAsCbor

lenderNftShortBs :: ValidatorHash -> SBS.ShortByteString
lenderNftShortBs = SBS.toShort . LB.toStrict . scriptAsCbor
