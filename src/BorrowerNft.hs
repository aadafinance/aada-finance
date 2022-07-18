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

module BorrowerNft where

import           Cardano.Api.Shelley      (PlutusScript (..), PlutusScriptV1)
import           Codec.Serialise
import qualified Data.ByteString.Lazy     as LB
import qualified Data.ByteString.Short    as SBS
import           Ledger                   hiding (singleton)
import qualified Ledger.Typed.Scripts     as Scripts
import           Ledger.Value             as Value
import qualified PlutusTx
import           PlutusTx.Prelude         hiding (Semigroup (..), unless)

{-# INLINABLE flattenBuiltinByteString #-}
flattenBuiltinByteString :: [BuiltinByteString] -> BuiltinByteString
flattenBuiltinByteString [] = emptyByteString
flattenBuiltinByteString (x:xs) = appendByteString x $ flattenBuiltinByteString xs

{-# INLINABLE borrower #-}
borrower :: TokenName
borrower = TokenName { unTokenName = flattenBuiltinByteString [consByteString x emptyByteString | x <- [66]]}  -- B

{-# INLINABLE mkPolicy #-}
mkPolicy :: TxOutRef -> Integer -> ScriptContext -> Bool
mkPolicy utxo _ ctx = validate
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    hasUTxO :: Bool
    hasUTxO = traceIfFalse "No minting policy specified utxo found" $ any (\i -> txInInfoOutRef i == utxo) $ txInfoInputs info

    validateMint :: (CurrencySymbol, TokenName, Integer) -> Bool
    validateMint (_, _, n) = hasUTxO && traceIfFalse "invalid mint amount" (n == 1)

    validateBurn :: (CurrencySymbol, TokenName, Integer) -> Bool
    validateBurn (_, _, n) = traceIfFalse "invalid burn amount" (n == (-1))

    ownNftFilter :: (CurrencySymbol, TokenName, Integer) -> Bool
    ownNftFilter (cs, tn, _) = cs == ownCurrencySymbol ctx && tn == borrower

    mintedFlattened :: [(CurrencySymbol, TokenName, Integer)]
    mintedFlattened = filter ownNftFilter $ flattenValue $ txInfoMint info

    validate :: Bool
    validate = validateMint (head mintedFlattened) ||
               validateBurn (head mintedFlattened)

policy :: TxOutRef -> Scripts.MintingPolicy
policy utxo = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy . mkPolicy ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode utxo

plutusScript :: TxOutRef -> Script
plutusScript = unMintingPolicyScript . policy

validator :: TxOutRef ->  Validator
validator = Validator . plutusScript

scriptAsCbor :: TxOutRef ->  LB.ByteString
scriptAsCbor = serialise . validator

borrowerNft :: TxOutRef ->  PlutusScript PlutusScriptV1
borrowerNft = PlutusScriptSerialised . SBS.toShort . LB.toStrict . scriptAsCbor

borrowerNftShortBs :: TxOutRef ->  SBS.ShortByteString
borrowerNftShortBs = SBS.toShort . LB.toStrict . scriptAsCbor
