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

module BorrowerNft where

import           Codec.Serialise
import qualified Data.ByteString.Lazy     as LB
import qualified Data.ByteString.Short    as SBS
import qualified PlutusTx
import           PlutusTx.Prelude         hiding (Semigroup (..), unless)
import Plutus.V2.Ledger.Api
import Plutus.V2.Ledger.Contexts
import Plutus.V1.Ledger.Value
import PlutusTx (unsafeFromBuiltinData)
import qualified Plutus.V1.Ledger.Scripts as Scripts

{-# INLINABLE flattenBuiltinByteString #-}
flattenBuiltinByteString :: [BuiltinByteString] -> BuiltinByteString
flattenBuiltinByteString [] = emptyByteString
flattenBuiltinByteString (x:xs) = appendByteString x $ flattenBuiltinByteString xs

{-# INLINABLE borrower #-}
borrower :: TokenName
borrower = TokenName { unTokenName = flattenBuiltinByteString [consByteString x emptyByteString | x <- [66]]}  -- B

{-# INLINABLE mkPolicy #-}
mkPolicy :: TxOutRef -> BuiltinData -> BuiltinData -> ()
mkPolicy utxo _ (unsafeFromBuiltinData -> ctx :: ScriptContext) = check validate
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    hasUTxO :: Bool
    hasUTxO = traceIfFalse "No minting policy specified utxo found" $ any (\i -> txInInfoOutRef i == utxo) $ txInfoInputs info

    validateMint :: (CurrencySymbol, TokenName, Integer) -> Bool
    validateMint (_, _, n) = hasUTxO && (traceIfFalse "invalid mint amount" $ n == 1)

    validateBurn :: (CurrencySymbol, TokenName, Integer) -> Bool
    validateBurn (_, _, n) = traceIfFalse "invalid burn amount" (n == (-1))

    ownNftFilter :: (CurrencySymbol, TokenName, Integer) -> Bool
    ownNftFilter (cs, tn, _) = cs == ownCurrencySymbol ctx && tn == borrower

    mintedFlattened :: [(CurrencySymbol, TokenName, Integer)]
    mintedFlattened = filter ownNftFilter $ flattenValue $ txInfoMint info

    validate :: Bool
    validate = validateMint (head mintedFlattened) ||
               validateBurn (head mintedFlattened)

borrowerNft :: TxOutRef -> Script
borrowerNft utxo = fromCompiledCode $
    $$(PlutusTx.compile [|| mkPolicy ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode utxo
