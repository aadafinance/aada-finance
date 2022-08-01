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
  , CollateralRedeemer(..)
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
    } deriving (Show, Generic, ToJSON, FromJSON)

data CollateralRedeemer = CollateralRedeemer
  { mintdate        :: !POSIXTime
  , interestPayDate :: !POSIXTime
  } deriving (Show, Generic, ToJSON, FromJSON)

data ContractInfo = ContractInfo
    { borrower     :: !TokenName
    , lenderNftCs  :: !CurrencySymbol
    , interestscvh :: !ValidatorHash
    , timeNft      :: !CurrencySymbol
    } deriving (Show, Generic, ToJSON, FromJSON)

{-# INLINABLE mkValidator #-}
mkValidator :: ContractInfo -> CollateralDatum -> CollateralRedeemer -> ScriptContext -> Bool
mkValidator contractInfo@ContractInfo{..} dat rdm ctx = validate
  where
    getLoanAmnt :: Value -> Integer
    getLoanAmnt v = assetClassValueOf v (loan dat)

    getInterestAmnt :: Value -> Integer
    getInterestAmnt v = assetClassValueOf v (interest dat)

    validateDebtAmnt :: Bool
    validateDebtAmnt = getLoanAmnt (U.valueToSc interestscvh ctx) >= loanamnt dat

    interestPercentage :: Integer
    interestPercentage = case (mintdate rdm + repayinterval dat) < interestPayDate rdm of
      True  -> 100
      False -> (getPOSIXTime (repayinterval dat) `multiplyInteger` 100) `divideInteger` getPOSIXTime loanHeld
       where
         loanHeld = interestPayDate rdm - mintdate rdm

    validateInterestAmnt :: Bool
    validateInterestAmnt = getInterestAmnt (U.valueToSc interestscvh ctx) >= ((interestamnt dat `multiplyInteger` 100) `divideInteger` interestPercentage)

    validateDebtAndInterestAmnt :: Bool
    validateDebtAndInterestAmnt = interest dat /= loan dat || (getLoanAmnt (U.valueToSc interestscvh ctx) >= loanamnt dat + interestamnt dat)

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
    doesNotContainAdditionalTokens = length (flattenValue $ U.valueToSc interestscvh ctx) < 4

    validateBorrower :: Bool
    validateBorrower = traceIfFalse "invalid debt amount sent to interest sc" validateDebtAmnt &&
                       traceIfFalse "invalid interest amount sent to interest sc" validateInterestAmnt &&
                       traceIfFalse "invalid debt and interest amount" validateDebtAndInterestAmnt &&
                       traceIfFalse "borrower nft is not burnt" validateBorrowerNftBurn &&
                       traceIfFalse "borrower deadline check fail" checkBorrowerDeadLine &&
                       traceIfFalse "invalid time nft token name" checkMintTnName &&
                       traceIfFalse "datum was not passed on" ownInputHash &&
                       traceIfFalse "too many tokens sent" doesNotContainAdditionalTokens

    checkDeadline :: Bool
    checkDeadline = traceIfFalse "deadline check fail" (contains (from (mintdate rdm + repayinterval dat)) (U.range ctx))

    checkBorrowerDeadLine :: Bool
    checkBorrowerDeadLine = traceIfFalse "borrower deadline check fail" (contains (U.range ctx) (from (interestPayDate rdm)))

    tokenNameIsCorrect :: TokenName -> Bool
    tokenNameIsCorrect tn = equalsByteString (unTokenName tn) (U.intToByteString $ getPOSIXTime (mintdate rdm))

    getTimeTokenName :: Maybe TokenName
    getTimeTokenName = case U.ownValue ctx of
      Just v -> (\(_, tn, _) -> tn) <$> find (\(cs, _, n) -> cs == timeNft && n == 1) (flattenValue v)
      Nothing -> Nothing

    checkMintTnName :: Bool
    checkMintTnName = traceIfFalse "invalid time token name" (maybe False tokenNameIsCorrect getTimeTokenName)

    checkLNftsAreBurnt :: Bool
    checkLNftsAreBurnt = traceIfFalse "Lender Nft not burnt" (any (\(cs, tn, n) -> cs == lenderNftCs && tn == lenderNftTn dat && n == (-1)) (U.mintFlattened ctx))

    checkForLiquidationNft :: Bool
    checkForLiquidationNft = traceIfFalse "liqudation token was not found" (any (\(cs, _, _) -> cs == liquidateNft dat) (U.mintFlattened ctx))

    validateLender :: Bool
    validateLender = checkLNftsAreBurnt && (checkDeadline && checkMintTnName || checkForLiquidationNft)

    validate :: Bool
    validate = validateLender ||
               validateBorrower

data Collateral
instance Scripts.ValidatorTypes Collateral where
    type instance DatumType Collateral = CollateralDatum
    type instance RedeemerType Collateral = CollateralRedeemer

collateralTypedValidator :: ContractInfo -> Scripts.TypedValidator Collateral
collateralTypedValidator contractInfo = Scripts.mkTypedValidator @Collateral
    ($$(PlutusTx.compile [|| mkValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode contractInfo)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @CollateralDatum @CollateralRedeemer

validator :: ContractInfo -> Validator
validator = Scripts.validatorScript . collateralTypedValidator

PlutusTx.makeIsDataIndexed ''CollateralDatum [('CollateralDatum, 0)]
PlutusTx.makeIsDataIndexed ''ContractInfo [('ContractInfo, 1)]
PlutusTx.makeIsDataIndexed ''CollateralRedeemer [('CollateralRedeemer, 0)]
PlutusTx.makeLift ''ContractInfo

scriptAsCbor :: ContractInfo -> LBS.ByteString
scriptAsCbor = serialise . validator

collateralScript :: ContractInfo -> PlutusScript PlutusScriptV1
collateralScript = PlutusScriptSerialised . collateralShortBs

collateralShortBs :: ContractInfo -> SBS.ShortByteString
collateralShortBs = SBS.toShort . LBS.toStrict . scriptAsCbor

collateralAddress :: ContractInfo -> Ledger.Address
collateralAddress = Ledger.scriptAddress . validator