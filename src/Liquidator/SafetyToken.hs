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

module Liquidator.SafetyToken
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
import Plutus.V1.Ledger.Api
import Liquidator.StRedeemer.StRedeemer

-- TODO refactor
{-# INLINABLE mkPolicy #-}
mkPolicy :: CurrencySymbol -> STRedeemer -> ScriptContext -> Bool
mkPolicy lenderNftCs rdm ctx =
  case mintedValue of
    [(cs, tn, n)] -> cs == ownCurrencySymbol ctx &&
      if n == 1 then
          validateMint cs tn
        else if n == (-1) then
          validateBurn tn
        else False
    _             -> False
  where
    mintFlattened :: [(CurrencySymbol, TokenName, Integer)]
    mintFlattened = flattenValue $ txInfoMint (scriptContextTxInfo ctx)

    mintedValue :: [(CurrencySymbol, TokenName, Integer)]
    mintedValue = filter (\(cs, _tn, _n) -> cs == ownCurrencySymbol ctx) mintFlattened

    -- TODO move to Common.Utils
    findDatumHash' :: ToData a => a -> TxInfo -> Maybe DatumHash
    findDatumHash' datum info = findDatumHash (Datum $ toBuiltinData datum) info

    containsNewDatum :: TxOut -> TokenName -> Bool
    containsNewDatum txo safetyToken = findDatumHash' safetyToken (U.info ctx) == txOutDatumHash txo

    validateLenderNft :: TxOut -> TokenName -> CurrencySymbol -> TokenName -> Integer -> Bool
    validateLenderNft txo safetyToken cs tn n =
      cs == lenderNftCs &&
      n == 1 &&
      lenderNftSentToSafetyModule cs tn &&
      validateTxOuts (assetClass cs tn) safetyToken

    -- TODO rename to better names
    inputHasLenderNft :: TokenName -> TxOut -> Bool
    inputHasLenderNft safetyToken txin = any (\(cs, tn, n) -> validateLenderNft txin safetyToken cs tn n) $ (flattenValue . txOutValue) txin

    lenderNftSentToSafetyModule :: CurrencySymbol -> TokenName -> Bool
    lenderNftSentToSafetyModule cs tn =
      any (\x -> txOutAddress x == safetyModule rdm && valueOf (txOutValue x) cs tn == 1) $ txInfoOutputs (U.info ctx)

    validateTxOut :: AssetClass -> TokenName -> TxOut -> Bool
    validateTxOut lenderNft safetyToken txo =
      assetClassValueOf (txOutValue txo) lenderNft == 1 &&
      txOutAddress txo == safetyModule rdm &&
      containsNewDatum txo safetyToken

    validateTxOuts :: AssetClass -> TokenName -> Bool
    validateTxOuts lenderNft safetyToken = any (validateTxOut lenderNft safetyToken) $ txInfoOutputs (U.info ctx)

    validateTxIn :: TokenName -> TxOut -> Bool
    validateTxIn safetyToken txin =
      inputHasLenderNft safetyToken txin

    validateTxIns :: TokenName -> Bool
    validateTxIns safetyToken = any (validateTxIn safetyToken) $ txInInfoResolved <$> txInfoInputs (U.info ctx)

    validateMint :: CurrencySymbol -> TokenName -> Bool
    validateMint cs tn =
      U.hasUTxO (utxo rdm) ctx &&
      U.validateTokenName' tn (liquidateInterestScAddr rdm) (safetyModule rdm) (utxo rdm) &&
      validateTxIns tn

    inputHasLenderNft' :: TxOut -> Bool
    inputHasLenderNft' txin = any (\(cs, _tn, n) -> cs == lenderNftCs && n == 1) $ (flattenValue . txOutValue) txin

    validateBurnTxIn :: TokenName -> TxOut -> Bool
    validateBurnTxIn tn txin =
      (txOutAddress txin == safetyModule rdm &&
      inputHasLenderNft' txin) ||
      (txOutAddress txin == liquidateInterestScAddr rdm
      && U.validateTokenName' tn (liquidateInterestScAddr rdm) (safetyModule rdm) (utxo rdm))

    validateBurnTxIns :: TokenName -> Bool
    validateBurnTxIns tn = any (validateBurnTxIn tn) $ (txInInfoResolved <$> txInfoInputs (U.info ctx))

    validateBurn :: TokenName -> Bool
    validateBurn = validateBurnTxIns

policy :: CurrencySymbol -> Scripts.MintingPolicy
policy params = mkMintingPolicyScript $
   $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy . mkPolicy||])
   `PlutusTx.applyCode`
   PlutusTx.liftCode params

-- data SafetyModule
-- instance Scripts.ValidatorTypes SafetyModule where
--     type instance DatumType SafetyModule = AssetClass
--     type instance RedeemerType SafetyModule = LiquidationAction

-- typedValidator :: ContractInfo -> Scripts.TypedValidator SafetyModule
-- typedValidator contractInfo = Scripts.mkTypedValidator @SafetyModule
--     ($$(PlutusTx.compile [|| mkValidator ||])
--     `PlutusTx.applyCode`
--     PlutusTx.liftCode contractInfo)
--     $$(PlutusTx.compile [|| wrap ||])
--   where
--     wrap = Scripts.wrapValidator @AssetClass @LiquidationAction

plutusScript :: CurrencySymbol -> Script
plutusScript = unMintingPolicyScript . policy

validator :: CurrencySymbol -> Validator
validator = Validator . plutusScript

scriptAsCbor :: CurrencySymbol -> LB.ByteString
scriptAsCbor = serialise . validator

aadaNft :: CurrencySymbol -> PlutusScript PlutusScriptV1
aadaNft = PlutusScriptSerialised . SBS.toShort . LB.toStrict . scriptAsCbor

aadaNftShortBs :: CurrencySymbol -> SBS.ShortByteString
aadaNftShortBs = SBS.toShort . LB.toStrict . scriptAsCbor
