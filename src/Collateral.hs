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
mkValidator :: ContractInfo -> CollateralDatum -> POSIXTime -> ScriptContext -> Bool
mkValidator contractInfo@ContractInfo{..} dat interestPayDate ctx = validate
  where
    getLoanAmnt :: Value -> Integer
    getLoanAmnt v = assetClassValueOf v (loan dat)

    getInterestAmnt :: Value -> Integer
    getInterestAmnt v = assetClassValueOf v (interest dat)

    validateDebtAmnt :: TxOut -> Bool
    validateDebtAmnt txo = getLoanAmnt (txOutValue txo) >= loanamnt dat

    interestPercentage :: Integer
    interestPercentage = case (lendDate dat + repayinterval dat) < interestPayDate of
      True  -> 100
      False -> (getPOSIXTime loanHeld `multiplyInteger` 100) `divideInteger` getPOSIXTime (repayinterval dat)
       where
        loanHeld = interestPayDate - lendDate dat

    getPartialInterest :: Integer
    getPartialInterest = if interestPercentage > 0
      then (interestamnt dat `multiplyInteger` interestPercentage) `divideInteger` 100
      else 0

    validateInterestAmnt :: TxOut -> Bool
    validateInterestAmnt txo = getInterestAmnt (txOutValue txo) >= getPartialInterest

    validateDebtAndInterestAmnt :: TxOut -> Bool
    validateDebtAndInterestAmnt txo = interest dat /= loan dat || (getLoanAmnt (txOutValue txo) >= loanamnt dat + getPartialInterest)

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
    validateReturn = validateBorrowerNftBurn &&
                     checkBorrowerDeadLine &&
                     validateTxOuts

    checkDeadline :: Bool
    checkDeadline = contains (from (lendDate dat + repayinterval dat)) (U.range ctx)

    checkBorrowerDeadLine :: Bool
    checkBorrowerDeadLine = contains (U.range ctx) (from interestPayDate)

    checkLNftIsBurnt :: Bool
    checkLNftIsBurnt = valueOf (txInfoMint $ U.info ctx) lenderNftCs (lenderNftTn dat) == (-1)

    checkForLiquidationNft :: Bool
    checkForLiquidationNft = any (\(cs, _, _) -> cs == liquidateNft dat) (U.mintFlattened ctx)

    validateLiquidation :: Bool
    validateLiquidation = checkLNftIsBurnt && (checkDeadline || checkForLiquidationNft)

    validate :: Bool
    validate = validateLiquidation ||
               validateReturn

data Collateral
instance Scripts.ValidatorTypes Collateral where
    type instance DatumType Collateral = CollateralDatum
    type instance RedeemerType Collateral = POSIXTime

collateralTypedValidator :: ContractInfo -> Scripts.TypedValidator Collateral
collateralTypedValidator contractInfo = Scripts.mkTypedValidator @Collateral
    ($$(PlutusTx.compile [|| mkValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode contractInfo)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @CollateralDatum @POSIXTime

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