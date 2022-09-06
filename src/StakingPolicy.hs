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

module StakingPolicy
  ( stakingPolicy
  , stakingPolicyShortBs
  ) where

import           Cardano.Api.Shelley      (PlutusScript (..), PlutusScriptV1)
import           Codec.Serialise
import qualified Data.ByteString.Lazy     as LB
import qualified Data.ByteString.Short    as SBS
import           Ledger                   hiding (singleton)
import qualified Ledger.Typed.Scripts     as Scripts
import           Ledger.Value             as Value
import qualified PlutusTx
import           PlutusTx.Builtins        (modInteger)
import           PlutusTx.Prelude         hiding (Semigroup (..), unless)
import qualified Plutus.V1.Ledger.Scripts as Plutus
import           Prelude                  (Show)

{-# INLINABLE mkPolicy #-}
mkPolicy :: TxOutRef -> PubKeyHash -> Integer -> ScriptContext -> Bool
mkPolicy utxo pkh _ ctx = validate
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    hasUTxO :: Bool
    hasUTxO = traceIfFalse "Tx didn't consume nft policy utxo" $ any (\i -> txInInfoOutRef i == utxo) $ txInfoInputs info

    getMintAmount :: Integer
    getMintAmount = case flattenValue (txInfoMint info) of
        [(_, _, amt)] -> amt
        _             -> 0

    txSignedByPkh :: Bool
    txSignedByPkh = traceIfFalse "transaction not signed by policyid pkh 1" $ txSignedBy info pkh

    validateMint :: Bool
    validateMint = hasUTxO && txSignedByPkh

    validateBurn :: Bool
    validateBurn = True

    validate :: Bool
    validate
        | mintedAmount == 2    = traceIfFalse "mint validation failed" validateMint
        | mintedAmount == (-2) = validateBurn
        | otherwise            = traceIfFalse "invalid mint amount" False
      where
        mintedAmount = getMintAmount

policy :: TxOutRef -> PubKeyHash -> Scripts.MintingPolicy
policy utxo pkh = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \utxo' pkh' -> Scripts.wrapMintingPolicy $ mkPolicy utxo' pkh' ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode utxo
    `PlutusTx.applyCode`
    PlutusTx.liftCode pkh

plutusScript :: TxOutRef -> PubKeyHash -> Script
plutusScript utxo pkh = unMintingPolicyScript $ policy utxo pkh

validator :: TxOutRef -> PubKeyHash -> Validator
validator utxo pkh = Validator $ plutusScript utxo pkh

scriptAsCbor :: TxOutRef -> PubKeyHash -> LB.ByteString
scriptAsCbor utxo pkh = serialise $ validator utxo pkh

stakingPolicy :: TxOutRef -> PubKeyHash -> PlutusScript PlutusScriptV1
stakingPolicy utxo pkh = PlutusScriptSerialised $ SBS.toShort $ LB.toStrict $ scriptAsCbor utxo pkh

stakingPolicyShortBs :: TxOutRef -> PubKeyHash -> SBS.ShortByteString
stakingPolicyShortBs utxo pkh = SBS.toShort $ LB.toStrict $ scriptAsCbor utxo pkh