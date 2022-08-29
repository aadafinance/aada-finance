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
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}

module AadaNft
    ( aadaNftScript
    , aadaNftPolicy
    ) where

import           Codec.Serialise
import qualified Data.ByteString.Lazy     as LB
import qualified Data.ByteString.Short    as SBS
import qualified PlutusTx
import           PlutusTx.Prelude         hiding (Semigroup (..), unless)
import qualified Common.Utils             as U
import           Plutus.V2.Ledger.Tx
import           Plutus.V2.Ledger.Contexts hiding (TxOut(..))
import           Plutus.V1.Ledger.Value
import           Plutus.V1.Ledger.Scripts
import           PlutusTx (unsafeFromBuiltinData)
import           Plutus.Model.V1

{-# INLINABLE mkPolicy #-}
mkPolicy :: Bool -> BuiltinData -> BuiltinData -> ()
mkPolicy isLender
  (unsafeFromBuiltinData -> utxo :: TxOutRef)
  (unsafeFromBuiltinData -> ctx :: ScriptContext)
  = check $ case mintedValue of
    [(_cs, tn, n)] -> validateMint tn n
    _              -> False
  where
    mintFlattened :: [(CurrencySymbol, TokenName, Integer)]
    mintFlattened = flattenValue $ txInfoMint (scriptContextTxInfo ctx)

    mintedValue :: [(CurrencySymbol, TokenName, Integer)]
    mintedValue = filter (\(cs, _tn, _n) -> cs == ownCurrencySymbol ctx) mintFlattened

    calculateTokenNameHash :: BuiltinByteString
    calculateTokenNameHash =
      sha2_256 (consByteString (txOutRefIdx utxo) ((getTxId . txOutRefId) utxo))

    validateTokenName :: TokenName -> Bool
    validateTokenName tn = unTokenName tn == calculateTokenNameHash

    checkForOverflow :: Bool
    checkForOverflow = txOutRefIdx utxo < 256

    validateMint :: TokenName -> Integer -> Bool
    validateMint tn amount =
      U.hasUTxO utxo ctx &&
      amount == 1 &&
      validateTokenName tn &&
      checkForOverflow ||
      amount == (-1)

aadaNftScript :: Bool -> Script
aadaNftScript isLender = fromCompiledCode $
    ($$(PlutusTx.compile [|| mkPolicy ||]) `PlutusTx.applyCode` PlutusTx.liftCode isLender)

-- code below is for plutus-simple-model testing
aadaNftPolicy :: Bool -> TypedPolicy TxOutRef
aadaNftPolicy isLender = mkTypedPolicy $
    ($$(PlutusTx.compile [|| mkPolicy ||]) `PlutusTx.applyCode` PlutusTx.liftCode isLender)

-- policy :: Bool -> Scripts.MintingPolicy
-- policy isLender = mkMintingPolicyScript $
--    $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy . mkPolicy ||])
--    `PlutusTx.applyCode`
--    PlutusTx.liftCode isLender

-- plutusScript :: Bool -> Script
-- plutusScript = unMintingPolicyScript . policy

-- validator :: Bool -> Validator
-- validator = Validator . plutusScript

-- scriptAsCbor :: Bool -> LB.ByteString
-- scriptAsCbor = serialise . validator

-- aadaNft :: Bool -> PlutusScript PlutusScriptV1
-- aadaNft = PlutusScriptSerialised . SBS.toShort . LB.toStrict . scriptAsCbor

-- aadaNftShortBs :: Bool -> SBS.ShortByteString
-- aadaNftShortBs = SBS.toShort . LB.toStrict . scriptAsCbor
