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
  ( collateral
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
import Plutus.V1.Ledger.Api

data CollateralDatum = CollateralDatum
    { borrowersNFT          :: !CurrencySymbol
    , borrowersPkh          :: !PaymentPubKeyHash
    , loantn                :: !TokenName
    , loancs                :: !CurrencySymbol
    , loanamnt              :: !Integer
    , interesttn            :: !TokenName
    , interestcs            :: !CurrencySymbol
    , interestamnt          :: !Integer
    , collateralcs          :: !CurrencySymbol
    , repayinterval         :: !POSIXTime
    , liquidateNft          :: !CurrencySymbol
    , collateraltn          :: !TokenName -- collateral token name
    , collateralamnt        :: !Integer   -- amount of collateral
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
    getLoanAmnt v = valueOf v (loancs dat) (loantn dat)

    getInterestAmnt :: Value -> Integer
    getInterestAmnt v = valueOf v (interestcs dat) (interesttn dat)

    validateDebtAmnt :: TxOut -> Bool
    validateDebtAmnt txo = getLoanAmnt (txOutValue txo) >= loanamnt dat

    interestPercentage :: Integer
    interestPercentage = case (mintdate rdm + repayinterval dat) < interestPayDate rdm of
      True  -> 100
      False -> (getPOSIXTime (repayinterval dat) `multiplyInteger` 100) `divideInteger` getPOSIXTime loanHeld
       where
         loanHeld = interestPayDate rdm - mintdate rdm

    validateInterestAmnt :: TxOut -> Bool
    validateInterestAmnt txo = getInterestAmnt (txOutValue txo) >= ((interestamnt dat `multiplyInteger` 100) `divideInteger` interestPercentage)

    validateDebtAndInterestAmnt :: TxOut -> Bool
    validateDebtAndInterestAmnt txo = not ((interestcs dat == loancs dat) && (interesttn dat == loantn dat)) || (getLoanAmnt (txOutValue txo) >= loanamnt dat + interestamnt dat)

    validateBorrowerNftBurn :: Bool
    validateBorrowerNftBurn = any (\(cs, tn, n) -> cs == borrowersNFT dat && tn == borrower && n == (-1)) (U.mintFlattened ctx)

    findDatumHash' :: ToData a => a -> TxInfo -> Maybe DatumHash
    findDatumHash' datum info = findDatumHash (Datum $ toBuiltinData datum) info

    containsNewDatum :: TxOut -> Bool
    containsNewDatum txo = findDatumHash' (lenderNftTn dat) (U.info ctx) == txOutDatumHash txo

    destinationIsToInterestSc :: TxOut -> Bool
    destinationIsToInterestSc txo = case toValidatorHash $ txOutAddress txo of
      Just vh -> vh == interestscvh
      Nothing -> False

    txOutValidate :: TxOut -> Bool
    txOutValidate txo = containsNewDatum txo &&
                        destinationIsToInterestSc txo &&
                        validateDebtAmnt txo &&
                        validateInterestAmnt txo &&
                        validateDebtAndInterestAmnt txo

    validateTxOuts :: Bool
    validateTxOuts = any txOutValidate (txInfoOutputs $ U.info ctx)

    validateBorrower :: Bool
    validateBorrower = traceIfFalse "borrower nft is not burnt" validateBorrowerNftBurn &&
                       traceIfFalse "borrower deadline check fail" checkBorrowerDeadLine &&
                       traceIfFalse "invalid time nft token name" checkMintTnName &&
                       traceIfFalse "no correct utxo to interestsc found" validateTxOuts

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
    checkLNftsAreBurnt = traceIfFalse "Lender Nft not burnt" (valueOf (txInfoMint $ U.info ctx) lenderNftCs (lenderNftTn dat) == (-1))

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

collateral :: ContractInfo -> PlutusScript PlutusScriptV1
collateral = PlutusScriptSerialised . collateralShortBs

collateralShortBs :: ContractInfo -> SBS.ShortByteString
collateralShortBs = SBS.toShort . LBS.toStrict . scriptAsCbor

collateralAddress :: ContractInfo -> Ledger.Address
collateralAddress = Ledger.scriptAddress . validator