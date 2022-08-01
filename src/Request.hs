{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Request where

import           Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV1)

import           Codec.Serialise ( serialise )
import           Data.Aeson           (ToJSON, FromJSON)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import GHC.Generics (Generic)
import           Plutus.V1.Ledger.Value
import           Ledger.Address
import           Plutus.V1.Ledger.Time
import           Plutus.V1.Ledger.Scripts
import qualified Ledger.Typed.Scripts as Scripts

import           Prelude                 (Semigroup (..), Show (..))
import           PlutusTx.Prelude hiding (Semigroup (..))
import qualified PlutusTx
import           Ledger               hiding (singleton)
import qualified Plutus.V1.Ledger.Scripts as Plutus
import qualified Common.Utils             as U
import Plutus.V1.Ledger.Api

data RequestDatum = RequestDatum
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

data RequestCollateral = RequestCollateral {
    lenderTn    :: !TokenName
  , lendDateRdm :: !POSIXTime
}

PlutusTx.makeIsDataIndexed ''RequestDatum [('RequestDatum, 0)]
PlutusTx.makeLift ''RequestDatum
PlutusTx.makeIsDataIndexed ''RequestCollateral [('RequestCollateral, 0)]

data ContractInfo = ContractInfo
    { borrower       :: !TokenName
    , lenderNftCs    :: !CurrencySymbol
    , collateralcsvh :: !ValidatorHash
    } deriving (Show, Generic, ToJSON, FromJSON)

{-# INLINABLE mkValidator #-}
mkValidator :: ContractInfo -> RequestDatum -> RequestCollateral -> ScriptContext -> Bool
mkValidator contractInfo@ContractInfo{..} dat rdm ctx = validate
  where
    valueToBorrower :: Value
    valueToBorrower = valuePaidTo (U.info ctx) (unPaymentPubKeyHash $ borrowersPkh dat)

    borrowerGetsWhatHeWants :: Bool
    borrowerGetsWhatHeWants = assetClassValueOf valueToBorrower (loan dat) >= loanamnt dat

    validateMint :: Bool
    validateMint = case U.mintFlattened ctx of
      [(cs, tn, amt)] -> (cs == lenderNftCs) &&
                         (tn == lenderTn rdm) &&
                         (amt == 1)
      _               -> False

    validateBorrowerMint :: Bool
    validateBorrowerMint = case U.mintFlattened ctx of
      [(cs, tn, amt)] -> (cs == borrowersNFT dat) &&
                         (tn == borrower) &&
                         (amt == (-1))
      _               -> False

    findDatumHash' :: ToData a => a -> TxInfo -> Maybe DatumHash
    findDatumHash' datum info = findDatumHash (Datum $ toBuiltinData datum) info

    expectedNewDatum :: RequestDatum
    expectedNewDatum = dat { lenderNftTn = lenderTn rdm, lendDate = lendDateRdm rdm }

    validateExpiration :: Bool
    validateExpiration = after (requestExpiration dat) (U.range ctx)

    isItToCollateral :: TxOut -> Bool
    isItToCollateral txo = case toValidatorHash $ txOutAddress txo of
      Just vh -> vh == collateralcsvh
      _       -> False

    containsRequiredCollateralAmount :: TxOut -> Bool
    containsRequiredCollateralAmount txo = case U.ownValue ctx of
      Just v  -> assetClassValueOf v (collateral dat) >= assetClassValueOf (txOutValue txo) (collateral dat)
      Nothing -> False

    containsNewDatum :: TxOut -> Bool
    containsNewDatum txo = findDatumHash' expectedNewDatum (U.info ctx) == txOutDatumHash txo

    doesNotContainAdditionalTokens :: TxOut -> Bool
    doesNotContainAdditionalTokens txo = length ((flattenValue . txOutValue) txo) < 4

    txOutValidate :: TxOut -> Bool
    txOutValidate txo = isItToCollateral txo &&
                        containsRequiredCollateralAmount txo &&
                        containsNewDatum txo &&
                        doesNotContainAdditionalTokens txo

    validateTxOuts :: Bool
    validateTxOuts = any txOutValidate (txInfoOutputs $ U.info ctx)

    checkDeadline :: Bool
    checkDeadline = contains (from (lendDateRdm rdm)) (U.range ctx)

    validate :: Bool
    validate = validateTxOuts &&
               traceIfFalse "lender nft was not minted" validateMint &&
               traceIfFalse "borrower didn't receive the loan" borrowerGetsWhatHeWants &&
               traceIfFalse "Invalid lend date parameter passed" checkDeadline &&
               traceIfFalse "Loan request has expired or txValidTo wasn't set correctly" validateExpiration ||
               traceIfFalse "borrower nft wasn't burnt" validateBorrowerMint

data RequestDataTypes
instance Scripts.ValidatorTypes RequestDataTypes where
    type instance DatumType    RequestDataTypes = RequestDatum
    type instance RedeemerType RequestDataTypes = RequestCollateral

requestTypedValidator :: ContractInfo -> Scripts.TypedValidator RequestDataTypes
requestTypedValidator contractInfo = Scripts.mkTypedValidator @RequestDataTypes
    ($$(PlutusTx.compile [|| mkValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode contractInfo)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @RequestDatum @RequestCollateral

requestValidator :: ContractInfo -> Validator
requestValidator = Scripts.validatorScript . requestTypedValidator

PlutusTx.makeIsDataIndexed ''ContractInfo [('ContractInfo, 1)]
PlutusTx.makeLift ''ContractInfo

script :: ContractInfo -> Plutus.Script
script = Plutus.unValidatorScript . requestValidator

scriptAsCbor :: ContractInfo -> LBS.ByteString
scriptAsCbor = serialise . requestValidator

request :: ContractInfo -> PlutusScript PlutusScriptV1
request = PlutusScriptSerialised . requestShortBs

requestShortBs :: ContractInfo -> SBS.ShortByteString
requestShortBs = SBS.toShort . LBS.toStrict . scriptAsCbor

requestAddress :: ContractInfo -> Ledger.Address
requestAddress = Ledger.scriptAddress . requestValidator
