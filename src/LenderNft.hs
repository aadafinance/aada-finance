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

{-# INLINABLE lender #-}
lender :: TokenName
lender = TokenName { unTokenName = consByteString 76 emptyByteString }  -- L

{-# INLINABLE mkPolicy #-}
mkPolicy :: ValidatorHash -> TxOutRef -> Integer -> ScriptContext -> Bool
mkPolicy vh utxo _ ctx = validate
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    hasUTxO :: Bool
    hasUTxO = traceIfFalse "utxo specified in lender nft minting policy not found" (any (\i -> txInInfoOutRef i == utxo) $ txInfoInputs info)

    valueToCollateralSc :: Value
    valueToCollateralSc = foldr (\(_, y) acc -> y <> acc) (PlutusTx.Prelude.mempty :: Value) (scriptOutputsAt vh info)

    nftIsSentToCollateralSc :: Bool
    nftIsSentToCollateralSc = traceIfFalse "minted lender nft is not sent to collateral smart contract" (valueOf valueToCollateralSc (ownCurrencySymbol ctx) lender == 1)

    validateMint :: Integer -> Bool
    validateMint amount = hasUTxO &&
                          traceIfFalse "invalid lender nft minted amount" (amount == 2) &&
                          nftIsSentToCollateralSc

    validateBurn :: Integer -> Bool
    validateBurn amount = traceIfFalse "invalid lender nft burnt amount" (amount == (-2))

    validate :: Bool
    validate =
        let amount = valueOf (txInfoMint info) (ownCurrencySymbol ctx) lender
        in validateMint amount || validateBurn amount

policy :: ValidatorHash -> TxOutRef -> Scripts.MintingPolicy
policy vh utxo = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \vh' utxo' -> Scripts.wrapMintingPolicy $ mkPolicy vh' utxo'||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode vh
    `PlutusTx.applyCode`
    PlutusTx.liftCode utxo

plutusScript :: ValidatorHash -> TxOutRef -> Script
plutusScript vh utxo = unMintingPolicyScript $ policy vh utxo

validator :: ValidatorHash -> TxOutRef -> Validator
validator vh utxo = Validator $ plutusScript vh utxo

scriptAsCbor :: ValidatorHash -> TxOutRef -> LB.ByteString
scriptAsCbor vh utxo = serialise $ validator vh utxo

lenderNft :: ValidatorHash -> TxOutRef -> PlutusScript PlutusScriptV1
lenderNft vh utxo = PlutusScriptSerialised $ SBS.toShort $ LB.toStrict $ scriptAsCbor vh utxo

lenderNftShortBs :: ValidatorHash -> TxOutRef -> SBS.ShortByteString
lenderNftShortBs vh utxo = SBS.toShort $ LB.toStrict $ scriptAsCbor vh utxo
