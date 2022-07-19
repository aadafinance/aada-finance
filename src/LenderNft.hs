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
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}

module LenderNft
  ( lenderNft
  ) where

import           Codec.Serialise
import qualified Data.ByteString.Lazy     as LB
import qualified Data.ByteString.Short    as SBS
import qualified PlutusTx
import           PlutusTx (unsafeFromBuiltinData)
import           PlutusTx.Prelude         hiding (Semigroup (..), unless)
import           Plutus.V2.Ledger.Contexts
import qualified Plutus.V1.Ledger.Scripts as Plutus
import           Plutus.V1.Ledger.Scripts
import           Plutus.V1.Ledger.Value
import           Prelude                  (Semigroup (..), Show)

{-# INLINABLE flattenBuiltinByteString #-}
flattenBuiltinByteString :: [BuiltinByteString] -> BuiltinByteString
flattenBuiltinByteString [] = emptyByteString
flattenBuiltinByteString (x:xs) = appendByteString x $ flattenBuiltinByteString xs

{-# INLINABLE lender #-}
lender :: TokenName
lender = TokenName { unTokenName = flattenBuiltinByteString [consByteString x emptyByteString | x <- [76]]}  -- L

{-# INLINABLE mkPolicy #-}
mkPolicy :: ValidatorHash -> TxOutRef -> BuiltinData -> BuiltinData -> ()
mkPolicy vh utxo _ (unsafeFromBuiltinData -> ctx :: ScriptContext) = check validate
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    hasUTxO :: Bool
    hasUTxO = traceIfFalse "utxo specified in lender nft minting policy not found" (any (\i -> txInInfoOutRef i == utxo) $ txInfoInputs info)

    valueToCollateralSc :: Value
    valueToCollateralSc = foldr (\(_, y) acc -> y <> acc) (PlutusTx.Prelude.mempty :: Value) (scriptOutputsAt vh info)

    nftIsSentToCollateralSc :: Bool
    nftIsSentToCollateralSc = traceIfFalse "minted lender nft is not sent to collateral smart contract" (valueOf valueToCollateralSc (ownCurrencySymbol ctx) lender == 1)

    validateMint :: (CurrencySymbol, TokenName, Integer) -> Bool
    validateMint (_, _, n) = hasUTxO &&
                             traceIfFalse "invalid lender nft minted amount" (n == 2) &&
                             nftIsSentToCollateralSc

    validateBurn :: (CurrencySymbol, TokenName, Integer) -> Bool
    validateBurn (_, _, n) = traceIfFalse "invalid lender nft burnt amount" (n == (-2))

    ownNftFilter :: (CurrencySymbol, TokenName, Integer) -> Bool
    ownNftFilter (cs, tn, _) = cs == ownCurrencySymbol ctx && tn == lender

    mintedFlattened :: [(CurrencySymbol, TokenName, Integer)]
    mintedFlattened = filter ownNftFilter $ flattenValue $ txInfoMint info

    validate :: Bool
    validate = validateMint (head mintedFlattened) || validateBurn (head mintedFlattened)

lenderNft :: ValidatorHash -> TxOutRef -> Script
lenderNft vh utxo = fromCompiledCode $
    $$(PlutusTx.compile [|| mkPolicy ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode vh
    `PlutusTx.applyCode`
    PlutusTx.liftCode utxo
