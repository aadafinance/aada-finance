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

module DebtRequest where

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

import qualified Collateral

data DebtRequestDatum = DebtRequestDatum
    { borrowersNftTn        :: !TokenName
    , borrowersAddress      :: !Address
    , loan                  :: !AssetClass
    , loanAmnt              :: !Integer
    , interest              :: !AssetClass
    , interestAmnt          :: !Integer
    , collateral            :: !AssetClass
    , collateralAmnt        :: !Integer
    , loanDuration          :: !POSIXTime
    , liquidateNft          :: !CurrencySymbol
    , collateralFactor      :: !Integer
    , liquidationCommission :: !Integer
    , requestExpiration     :: !POSIXTime
    , lenderNftTn           :: !TokenName
    , lendDate              :: !POSIXTime
    } deriving (Show, Generic, ToJSON, FromJSON)

PlutusTx.makeIsDataIndexed ''DebtRequestDatum [('DebtRequestDatum, 0)]
PlutusTx.makeLift ''DebtRequestDatum

data ContractInfo = ContractInfo
    { lenderNftCs    :: !CurrencySymbol
    , borrowersNftCs :: !CurrencySymbol
    , collateralSc   :: !Address
    } deriving (Show, Generic, ToJSON, FromJSON)

{-# INLINABLE mkValidator #-}
mkValidator :: ContractInfo -> DebtRequestDatum -> TokenName -> ScriptContext -> Bool
mkValidator contractInfo@ContractInfo{..} dat borrowerTn ctx = validate
  where
    borrowerGetsWhatHeWants :: Bool
    borrowerGetsWhatHeWants =
      assetClassValueOf (U.valuePaidToAddress ctx (borrowersAddress dat)) (loan dat)
      == loanAmnt dat

    ownHashFilter :: Maybe ValidatorHash -> Bool
    ownHashFilter mvh = Just (ownHash ctx) == mvh

    txHasOneDebtRequestInputOnly :: Bool
    txHasOneDebtRequestInputOnly = length (filter ownHashFilter $ toValidatorHash . txOutAddress . txInInfoResolved <$> txInfoInputs (U.info ctx)) == 1

    txHasOneScInputOnly :: Bool
    txHasOneScInputOnly =
      length (filter isJust $ toValidatorHash . txOutAddress . txInInfoResolved <$> txInfoInputs (U.info ctx)) == 1

    validateMint :: Bool
    validateMint = case U.mintFlattened ctx of
      [(cs, tn, amt)] -> (cs == borrowersNftCs) &&
                         (tn == borrowerTn) &&
                         (amt == 1)
      _               -> False

    findDatumHash' :: ToData a => a -> TxInfo -> Maybe DatumHash
    findDatumHash' datum info = findDatumHash (Datum $ toBuiltinData datum) info

    expectedNewDatum :: POSIXTime -> Collateral.CollateralDatum
    expectedNewDatum ld = Collateral.CollateralDatum {
        Collateral.borrowersNftTn        = borrowerTn
      , Collateral.borrowersAddress      = borrowersAddress dat
      , Collateral.loan                  = loan dat
      , Collateral.loanAmnt              = loanAmnt dat
      , Collateral.interest              = interest dat
      , Collateral.interestAmnt          = interestAmnt dat
      , Collateral.collateral            = collateral dat
      , Collateral.collateralAmnt        = collateralAmnt dat
      , Collateral.loanDuration          = loanDuration dat
      , Collateral.liquidateNft          = liquidateNft dat
      , Collateral.collateralFactor      = collateralFactor dat
      , Collateral.liquidationCommission = liquidationCommission dat
      , Collateral.requestExpiration     = requestExpiration dat
      , Collateral.lenderNftTn           = lenderNftTn dat
      , Collateral.lendDate              = ld
    }

    validateExpiration :: Bool
    validateExpiration = after (requestExpiration dat) (U.range ctx)

    isItToCollateral :: TxOut -> Bool
    isItToCollateral txo = txOutAddress txo == collateralSc

    containsRequiredCollateralAmount :: TxOut -> Bool
    containsRequiredCollateralAmount txo =
      collateralAmnt dat <= assetClassValueOf (txOutValue txo) (collateral dat)

    containsNewDatum :: TxOut -> Bool
    containsNewDatum txo = case U.getUpperBound ctx of
      Just ub -> findDatumHash' (expectedNewDatum ub) (U.info ctx) == txOutDatumHash txo
      Nothing -> False

    checkForTokensDos :: TxOut -> Bool
    checkForTokensDos txo = length ((flattenValue . txOutValue) txo) <= 3

    txOutValidate :: TxOut -> Bool
    txOutValidate txo =
      isItToCollateral txo &&
      containsRequiredCollateralAmount txo &&
      containsNewDatum txo &&
      checkForTokensDos txo

    validateTxOuts :: Bool
    validateTxOuts = any txOutValidate (txInfoOutputs $ U.info ctx)

    validateCancelDebtRequest :: Bool
    validateCancelDebtRequest = case U.mintFlattened ctx of
      [(cs, tn, amt)] -> (cs == lenderNftCs) &&
                         (tn == lenderNftTn dat) &&
                         (amt == (-1))
      _               -> False

    validate :: Bool
    validate =
      validateTxOuts &&
      validateMint &&
      txHasOneDebtRequestInputOnly &&
      txHasOneScInputOnly &&
      validateExpiration ||
      validateCancelDebtRequest

data RequestDataTypes
instance Scripts.ValidatorTypes RequestDataTypes where
    type instance DatumType    RequestDataTypes = DebtRequestDatum
    type instance RedeemerType RequestDataTypes = TokenName

debtRequestTypedValidator :: ContractInfo -> Scripts.TypedValidator RequestDataTypes
debtRequestTypedValidator contractInfo = Scripts.mkTypedValidator @RequestDataTypes
    ($$(PlutusTx.compile [|| mkValidator ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode contractInfo)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @DebtRequestDatum @TokenName

debtRequestValidator :: ContractInfo -> Validator
debtRequestValidator = Scripts.validatorScript . debtRequestTypedValidator

PlutusTx.makeIsDataIndexed ''ContractInfo [('ContractInfo, 1)]
PlutusTx.makeLift ''ContractInfo

script :: ContractInfo -> Plutus.Script
script = Plutus.unValidatorScript . debtRequestValidator

scriptAsCbor :: ContractInfo -> LBS.ByteString
scriptAsCbor = serialise . debtRequestValidator

debtRequest :: ContractInfo -> PlutusScript PlutusScriptV1
debtRequest = PlutusScriptSerialised . debtRequestShortBs

debtRequestShortBs :: ContractInfo -> SBS.ShortByteString
debtRequestShortBs = SBS.toShort . LBS.toStrict . scriptAsCbor

debtRequestAddress :: ContractInfo -> Ledger.Address
debtRequestAddress = Ledger.scriptAddress . debtRequestValidator
