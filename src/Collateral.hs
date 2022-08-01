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
import PlutusTx.Builtins (equalsByteString, divideInteger, multiplyInteger)
import qualified Common.Utils             as U

data CollateralDatum = CollateralDatum
    { borrowersNFT          :: !CurrencySymbol
    , borrowersPkh          :: !PaymentPubKeyHash
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
    { borrower     :: !TokenName
    , lenderNftCs  :: !CurrencySymbol
    , interestscvh :: !ValidatorHash
    } deriving (Show, Generic, ToJSON, FromJSON)

{-# INLINABLE mkValidator #-}
mkValidator :: ContractInfo -> CollateralDatum -> POSIXTime -> ScriptContext -> Bool
mkValidator contractInfo@ContractInfo{..} dat interestPayDate ctx = validate
  where
    getLoanAmnt :: Value -> Integer
    getLoanAmnt v = assetClassValueOf v (loan dat)

    getInterestAmnt :: Value -> Integer
    getInterestAmnt v = assetClassValueOf v (interest dat)

    validateDebtAmnt :: Bool
    validateDebtAmnt = getLoanAmnt (U.valueToSc interestscvh ctx) >= loanamnt dat

    interestPercentage :: Integer
    interestPercentage = case (lendDate dat + repayinterval dat) < interestPayDate of
      True  -> 100
      False -> (getPOSIXTime (repayinterval dat) `multiplyInteger` 100) `divideInteger` getPOSIXTime loanHeld
       where
         loanHeld = interestPayDate - lendDate dat

    getPartialInterest :: Integer
    getPartialInterest = (interestamnt dat `multiplyInteger` 100) `divideInteger` interestPercentage

    validateInterestAmnt :: Bool
    validateInterestAmnt = getInterestAmnt (U.valueToSc interestscvh ctx) >= getPartialInterest

    validateDebtAndInterestAmnt :: Bool
    validateDebtAndInterestAmnt = interest dat /= loan dat || (getLoanAmnt (U.valueToSc interestscvh ctx) >= loanamnt dat + getPartialInterest)

    validateBorrowerNftBurn :: Bool
    validateBorrowerNftBurn = any (\(cs, tn, n) -> cs == borrowersNFT dat && tn == borrower && n == (-1)) (U.mintFlattened ctx)

    getCollateralScHashes :: [DatumHash]
    getCollateralScHashes = map fst (scriptOutputsAt interestscvh (U.info ctx))

    validateOutputHash :: DatumHash -> Bool
    validateOutputHash h = h `elem` getCollateralScHashes

    ownInputHash :: Bool
    ownInputHash = case U.ownInput ctx of
      Just txin -> maybe False validateOutputHash (txOutDatumHash txin)
      Nothing   -> False

    doesNotContainAdditionalTokens :: Bool
    doesNotContainAdditionalTokens = length (flattenValue $ U.valueToSc interestscvh ctx) <= 3

    validateReturn :: Bool
    validateReturn = traceIfFalse "invalid debt amount sent to interest sc" validateDebtAmnt &&
                     ((interestPayDate < lendDate dat) || (traceIfFalse "invalid interest amount sent to interest sc" validateInterestAmnt &&
                     traceIfFalse "invalid debt and interest amount" validateDebtAndInterestAmnt)) &&
                     traceIfFalse "borrower nft is not burnt" validateBorrowerNftBurn &&
                     traceIfFalse "borrower deadline check fail" checkBorrowerDeadLine &&
                     traceIfFalse "datum was not passed on" ownInputHash &&
                     traceIfFalse "too many tokens sent" doesNotContainAdditionalTokens

    checkDeadline :: Bool
    checkDeadline = traceIfFalse "deadline check fail" (contains (from (lendDate dat + repayinterval dat)) (U.range ctx))

    checkBorrowerDeadLine :: Bool
    checkBorrowerDeadLine = traceIfFalse "borrower deadline check fail" (contains (U.range ctx) (from interestPayDate))

    checkLNftsAreBurnt :: Bool
    checkLNftsAreBurnt = traceIfFalse "Lender Nft not burnt" (any (\(cs, tn, n) -> cs == lenderNftCs && tn == lenderNftTn dat && n == (-1)) (U.mintFlattened ctx))

    checkForLiquidationNft :: Bool
    checkForLiquidationNft = traceIfFalse "liqudation token was not found" (any (\(cs, _, _) -> cs == liquidateNft dat) (U.mintFlattened ctx))

    validateLiquidation :: Bool
    validateLiquidation = checkLNftsAreBurnt && (checkDeadline || checkForLiquidationNft)

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