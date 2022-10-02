{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Liquidator.SafetyModule
  ( safetyScript
  , safetyShortBs
  , validator
  , typedValidator
  , safetyAddress
  , LiquidationAction(..)
  , ContractInfo(..)
  ) where

import           Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV1)

import           Codec.Serialise
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS

import           Plutus.V1.Ledger.Contexts
import qualified Plutus.V1.Ledger.Scripts as Plutus
import           Plutus.V1.Ledger.Scripts
import           Plutus.V1.Ledger.Value
import qualified PlutusTx
import           PlutusTx.Prelude hiding (Semigroup (..), unless)
import           Prelude              (Show (..))
import           Ledger.Typed.Scripts as Scripts
import           Ledger
import qualified Common.Utils             as U
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import qualified Collateral as C
import Plutus.V1.Ledger.Api (UnsafeFromData(unsafeFromBuiltinData))
import PlutusTx.Builtins (divideInteger, multiplyInteger)

data LiquidationAction = Cancel | LiquidateByDeadline | LiquidateWithOracle
  deriving (Generic, ToJSON, FromJSON)

PlutusTx.makeIsDataIndexed ''LiquidationAction [ ('Cancel,    0)
                                               , ('LiquidateByDeadline, 1)
                                               , ('LiquidateWithOracle, 2)
                                               ]
PlutusTx.makeLift ''LiquidationAction

data ContractInfo = ContractInfo
    { lenderNftCs   :: !CurrencySymbol
    , collateralSc  :: !Address
    , interestSc    :: !Address
    , safetyTokenCs :: !CurrencySymbol
    } deriving (Show, Generic, ToJSON, FromJSON)

PlutusTx.makeIsDataIndexed ''ContractInfo [('ContractInfo, 1)]
PlutusTx.makeLift ''ContractInfo

{-# INLINABLE mkValidator #-}
mkValidator :: ContractInfo -> TokenName -> LiquidationAction -> ScriptContext -> Bool
mkValidator contractInfo@ContractInfo{..} safetyTokenTn action ctx =
  case action of
    Cancel    -> validateCancelRequest
    LiquidateByDeadline -> validateLiquidate
    LiquidateWithOracle -> validateLiquidate
  where
    getCollateralDatum :: Datum -> C.CollateralDatum
    getCollateralDatum datum = unsafeFromBuiltinData (getDatum datum)

    getOwnLockedDatum :: Maybe DatumHash
    getOwnLockedDatum = (txOutDatumHash . txInInfoResolved) =<< findOwnInput ctx
--  case (txOutDatumHash . txInInfoResolved ) <$> (findOwnInput ctx) of
--       Just mbdh -> mbdh
--       Nothing -> Nothing

    findDatumHash' :: PlutusTx.ToData a => a -> TxInfo -> Maybe DatumHash
    findDatumHash' datum info = findDatumHash (Datum $ PlutusTx.toBuiltinData datum) info

    hasSafetyDatum :: TxOut -> Bool
    hasSafetyDatum txo =
      traceBool "datum is correct" "datum is not correct" $
      (txOutDatumHash txo == findDatumHash' safetyTokenTn (U.info ctx))

    calculateLiquidateCommission :: C.CollateralDatum -> Integer
    calculateLiquidateCommission dat =
      C.collateralAmnt dat `multiplyInteger` 1000000000000 `divideInteger` C.liquidationCommission dat

    collatSentToInterest :: TxOut -> C.CollateralDatum -> Bool
    collatSentToInterest txo dat = -- True
      traceBool "txout address IS to interestSc" "tx out address is not to interestsc" $ txOutAddress txo == interestSc -- &&
      -- assetClassValueOf (txOutValue txo) (C.collateral dat) == C.collateralAmnt dat - calculateLiquidateCommission dat

    validateTxOut :: C.CollateralDatum -> TxOut -> Bool
    validateTxOut dat txo =
      hasSafetyDatum txo &&
      collatSentToInterest txo dat

    validateTxOuts :: Datum -> Bool
    validateTxOuts datum = any (validateTxOut $ getCollateralDatum datum) (txInfoOutputs (U.info ctx))

    validateDatum :: TxOut -> Bool
    validateDatum txin = case txOutDatumHash txin of
        Just dh -> maybe False validateTxOuts (findDatum dh (U.info ctx))
        Nothing -> False

    validateTxIn :: TxOut -> Bool
    validateTxIn txin =
      txOutAddress txin == collateralSc &&
      validateDatum txin

    validateLiquidate :: Bool
    validateLiquidate = any validateTxIn (txInInfoResolved <$> txInfoInputs (U.info ctx))

    containsSafetyToken :: TxOut -> Bool
    containsSafetyToken txin = valueOf (txOutValue txin) safetyTokenCs safetyTokenTn == 1

    safetyTokenIsBurnt :: Bool
    safetyTokenIsBurnt = case U.mintFlattened ctx of
      [(cs, tn, n)] -> (n == (-1)) && assetClass cs tn == assetClass safetyTokenCs safetyTokenTn
      _             -> False

    validateCancelRequest :: Bool
    validateCancelRequest =
      any containsSafetyToken (txInfoOutputs (U.info ctx)) &&
      safetyTokenIsBurnt

data SafetyModule
instance Scripts.ValidatorTypes SafetyModule where
    type instance DatumType SafetyModule = TokenName
    type instance RedeemerType SafetyModule = LiquidationAction

typedValidator :: ContractInfo -> Scripts.TypedValidator SafetyModule
typedValidator contractInfo = Scripts.mkTypedValidator @SafetyModule
    ($$(PlutusTx.compile [|| mkValidator ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode contractInfo)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @TokenName @LiquidationAction

validator :: ContractInfo -> Validator
validator = Scripts.validatorScript . typedValidator

script :: ContractInfo -> Plutus.Script
script = Plutus.unValidatorScript . validator

safetyShortBs :: ContractInfo -> SBS.ShortByteString
safetyShortBs = SBS.toShort . LBS.toStrict . serialise . script

safetyScript :: ContractInfo -> PlutusScript PlutusScriptV1
safetyScript = PlutusScriptSerialised . safetyShortBs

safetyAddress :: ContractInfo -> Address
safetyAddress = scriptHashAddress . Scripts.validatorHash . typedValidator
