{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use guards" #-}

module Collateral
  ( collateralScript
  , collateralShortBs
  , ContractInfo(..)
  , collateralTypedValidator
  , CollateralDatum(..)
  , validator
  , collateralAddress
  ) where

import           Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV1)

import           Codec.Serialise
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS

import           Plutus.V1.Ledger.Contexts
import           Plutus.V1.Ledger.Scripts
import           Plutus.V1.Ledger.Value
import qualified PlutusTx
import           PlutusTx.Prelude hiding (Semigroup (..), unless)
import           Prelude              (Show (..))

import           Ledger.Typed.Scripts as Scripts
import           Ledger hiding (singleton)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import PlutusTx.Builtins (divideInteger, multiplyInteger)
import qualified Common.Utils             as U
import Plutus.V1.Ledger.Api

data CollateralDatum = CollateralDatum
    { borrowersNftTn        :: !TokenName
    , borrowersAddress      :: !Address
    , loan                  :: !AssetClass
    , loanamnt              :: !Integer
    , interest              :: !AssetClass
    , interestamnt          :: !Integer
    , collateral            :: !AssetClass
    , collateralamnt        :: !Integer
    , repayinterval         :: !POSIXTime
    , liquidateNft          :: !CurrencySymbol
    , collateralFactor      :: !Integer   -- Colalteral factor used for liquidation
    , liquidationCommission :: !Integer   -- How much % borrower will pay for lender when liquidated (before time passes)
    , requestExpiration     :: !POSIXTime
    , lenderNftTn           :: !TokenName
    , lendDate              :: !POSIXTime
    } deriving (Show, Generic, ToJSON, FromJSON)

data ContractInfo = ContractInfo
    { lenderNftCs    :: !CurrencySymbol
    , borrowersNftCs :: !CurrencySymbol
    , interestSc     :: !Address
    } deriving (Show, Generic, ToJSON, FromJSON)

{-# INLINABLE mkValidator #-}
mkValidator :: ContractInfo -> CollateralDatum -> Integer -> ScriptContext -> Bool
mkValidator contractInfo@ContractInfo{..} dat _ ctx = validate
  where
    getLoanAmnt :: Value -> Integer
    getLoanAmnt v = assetClassValueOf v (loan dat)

    getInterestAmnt :: Value -> Integer
    getInterestAmnt v = assetClassValueOf v (interest dat)

    validateDebtAmnt :: TxOut -> Bool
    validateDebtAmnt txo = getLoanAmnt (txOutValue txo) >= loanamnt dat

    getPartialInterest :: POSIXTime -> Integer
    getPartialInterest interestPayDate = if repayIntEnd > interestPayDate && denominator > 0
      then
        (loanHeld `multiplyInteger` interestamnt dat) `divideInteger` denominator
      else
        interestamnt dat
      where
        denominator = getPOSIXTime (repayinterval dat)
        repayIntEnd = lendDate dat + repayinterval dat
        loanHeld = getPOSIXTime $ interestPayDate - lendDate dat

    validateInterestAmnt :: TxOut -> Bool
    validateInterestAmnt txo = case U.getUpperBound ctx of
      Just interestPayDate -> getInterestAmnt (txOutValue txo) >= getPartialInterest interestPayDate
      Nothing              -> False

    validateDebtAndInterestAmnt :: TxOut -> Bool
    validateDebtAndInterestAmnt txo = case U.getUpperBound ctx of
      Just interestPayDate -> interest dat /= loan dat ||
                              (getLoanAmnt (txOutValue txo) >= loanamnt dat + getPartialInterest interestPayDate)
      Nothing              -> False

    validateBorrowerNftBurn :: Bool
    validateBorrowerNftBurn = any (\(cs, tn, n) -> cs == borrowersNftCs && tn == borrowersNftTn dat && n == (-1)) (U.mintFlattened ctx)

    findDatumHash' :: ToData a => a -> TxInfo -> Maybe DatumHash
    findDatumHash' datum info = findDatumHash (Datum $ toBuiltinData datum) info

    containsNewDatum :: TxOut -> Bool
    containsNewDatum txo = findDatumHash' (lenderNftTn dat) (U.info ctx) == txOutDatumHash txo

    destinationIsToInterestSc :: TxOut -> Bool
    destinationIsToInterestSc txo = txOutAddress txo == interestSc

    txOutValidate :: TxOut -> Bool
    txOutValidate txo = containsNewDatum txo &&
                        destinationIsToInterestSc txo &&
                        validateDebtAmnt txo &&
                        validateInterestAmnt txo &&
                        validateDebtAndInterestAmnt txo &&
                        checkForTokensDos txo

    validateTxOuts :: Bool
    validateTxOuts = any txOutValidate (txInfoOutputs $ U.info ctx)

    checkForTokensDos :: TxOut -> Bool
    checkForTokensDos txo = length ((flattenValue . txOutValue) txo) <= 3

    validateReturn :: Bool
    validateReturn = traceIfFalse "borrower nft is not burnt" validateBorrowerNftBurn &&
                     traceIfFalse "no correct utxo to interestsc found" validateTxOuts

    checkDeadline :: Bool
    checkDeadline = traceIfFalse "deadline check fail" (contains (from (lendDate dat + repayinterval dat)) (U.range ctx))

    checkLNftIsBurnt :: Bool
    checkLNftIsBurnt = traceIfFalse "Lender Nft not burnt" (valueOf (txInfoMint $ U.info ctx) lenderNftCs (lenderNftTn dat) == (-1))

    checkForLiquidationToken :: Bool
    checkForLiquidationToken = any (\(cs, _, n) -> cs == liquidateNft dat && n > 0) (U.mintFlattened ctx)

    validateLiquidation :: Bool
    validateLiquidation = checkLNftIsBurnt && (checkDeadline || checkForLiquidationToken)

    validate :: Bool
    validate = validateLiquidation ||
               validateReturn

data Collateral
instance Scripts.ValidatorTypes Collateral where
    type instance DatumType Collateral = CollateralDatum
    type instance RedeemerType Collateral = Integer

collateralTypedValidator :: ContractInfo -> Scripts.TypedValidator Collateral
collateralTypedValidator contractInfo = Scripts.mkTypedValidator @Collateral
    ($$(PlutusTx.compile [|| mkValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode contractInfo)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @CollateralDatum @Integer

validator :: ContractInfo -> Validator
validator = Scripts.validatorScript . collateralTypedValidator

PlutusTx.makeIsDataIndexed ''CollateralDatum [('CollateralDatum, 0)]
PlutusTx.makeIsDataIndexed ''ContractInfo [('ContractInfo, 1)]
PlutusTx.makeLift ''ContractInfo

scriptAsCbor :: ContractInfo -> LBS.ByteString
scriptAsCbor = serialise . validator

collateralScript :: ContractInfo -> PlutusScript PlutusScriptV1
collateralScript = PlutusScriptSerialised . collateralShortBs

collateralShortBs :: ContractInfo -> SBS.ShortByteString
collateralShortBs = SBS.toShort . LBS.toStrict . scriptAsCbor

collateralAddress :: ContractInfo -> Ledger.Address
collateralAddress = Ledger.scriptAddress . validator