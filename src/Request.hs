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

import qualified Collateral

data RequestDatum = RequestDatum
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
    , collateralFactor      :: !Integer   -- Colalteral factor used for liquidation
    , liquidationCommission :: !Integer   -- How much % borrower will pay for lender when liquidated (before time passes)
    , requestExpiration     :: !POSIXTime
    , lenderNftTn           :: !TokenName
    , lendDate              :: !POSIXTime
    } deriving (Show, Generic, ToJSON, FromJSON)

PlutusTx.makeIsDataIndexed ''RequestDatum [('RequestDatum, 0)]
PlutusTx.makeLift ''RequestDatum

data ContractInfo = ContractInfo
    { lenderNftCs    :: !CurrencySymbol
    , borrowersNftCs :: !CurrencySymbol
    , collateralSc   :: !Address
    } deriving (Show, Generic, ToJSON, FromJSON)

{-# INLINABLE mkValidator #-}
mkValidator :: ContractInfo -> RequestDatum -> TokenName -> ScriptContext -> Bool
mkValidator contractInfo@ContractInfo{..} dat lenderTn ctx = validate
  where
    borrowerGetsWhatHeWants :: Bool
    borrowerGetsWhatHeWants =
      assetClassValueOf (U.valuePaidToAddress ctx (borrowersAddress dat)) (loan dat)
      == loanAmnt dat

    ownHashFilter :: Maybe ValidatorHash -> Bool
    ownHashFilter mvh = Just (ownHash ctx) == mvh

    txHasOneRequestInputOnly :: Bool
    txHasOneRequestInputOnly = length (filter ownHashFilter $ toValidatorHash . txOutAddress . txInInfoResolved <$> txInfoInputs (U.info ctx)) == 1

    txHasOneScInputOnly :: Bool
    txHasOneScInputOnly =
      length (filter isJust $ toValidatorHash . txOutAddress . txInInfoResolved <$> txInfoInputs (U.info ctx)) == 1

    validateMint :: Bool
    validateMint = case U.mintFlattened ctx of
      [(cs, tn, amt)] -> (cs == lenderNftCs) &&
                         (tn == lenderTn) &&
                         (amt == 1)
      _               -> False

    validateBorrowerMint :: Bool
    validateBorrowerMint = case U.mintFlattened ctx of
      [(cs, tn, amt)] -> (cs == borrowersNftCs) &&
                         (tn == borrowersNftTn dat) &&
                         (amt == (-1))
      _               -> False

    findDatumHash' :: ToData a => a -> TxInfo -> Maybe DatumHash
    findDatumHash' datum info = findDatumHash (Datum $ toBuiltinData datum) info

    expectedNewDatum :: POSIXTime -> Collateral.CollateralDatum
    expectedNewDatum ld = Collateral.CollateralDatum {
        Collateral.borrowersNftTn        = borrowersNftTn dat
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
      , Collateral.lenderNftTn           = lenderTn
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

    validate :: Bool
    validate =
      validateTxOuts &&
      validateMint &&
      borrowerGetsWhatHeWants &&
      txHasOneRequestInputOnly &&
      txHasOneScInputOnly &&
      validateExpiration ||
      validateBorrowerMint

data RequestDataTypes
instance Scripts.ValidatorTypes RequestDataTypes where
    type instance DatumType    RequestDataTypes = RequestDatum
    type instance RedeemerType RequestDataTypes = TokenName

requestTypedValidator :: ContractInfo -> Scripts.TypedValidator RequestDataTypes
requestTypedValidator contractInfo = Scripts.mkTypedValidator @RequestDataTypes
    ($$(PlutusTx.compile [|| mkValidator ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode contractInfo)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @RequestDatum @TokenName

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
