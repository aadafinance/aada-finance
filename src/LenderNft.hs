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
import qualified Plutus.V1.Ledger.Scripts as Plutus
import           Prelude                  (Semigroup (..), Show)
import qualified Common.Utils             as U

{-# INLINABLE mkPolicy #-}
mkPolicy :: ValidatorHash -> Integer -> ScriptContext -> Bool
mkPolicy vh _ ctx = validate
  where
    nftIsSentToCollateralSc :: Bool
    nftIsSentToCollateralSc = traceIfFalse "minted lender nft is not sent to collateral smart contract" (valueOf (U.valueToSc vh ctx) (ownCurrencySymbol ctx) lender == 1)

    validateMint :: Integer -> Bool
    validateMint amount = traceIfFalse "invalid lender nft minted amount" (amount == 2) &&
                          nftIsSentToCollateralSc

    validateBurn :: Integer -> Bool
    validateBurn amount = traceIfFalse "invalid lender nft burnt amount" (amount == (-2))

    validate :: Bool
    validate =
        let amount = valueOf (txInfoMint (U.info ctx)) (ownCurrencySymbol ctx) lender
        in validateMint amount || validateBurn amount

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
