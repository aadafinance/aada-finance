{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Request where

import           Codec.Serialise ( serialise )
import           Data.Aeson           (ToJSON, FromJSON)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import GHC.Generics (Generic)
import           Plutus.V1.Ledger.Value
import           Plutus.V1.Ledger.Interval
import           Plutus.V1.Ledger.Time
import           Plutus.V2.Ledger.Contexts
import           Plutus.V1.Ledger.Scripts
import           Plutus.V1.Ledger.Value
import           Plutus.V2.Ledger.Api
import           Plutus.V1.Ledger.Address

import           Prelude                 (Semigroup (..), Show (..))
import           PlutusTx.Prelude hiding (Semigroup (..))
import qualified PlutusTx
import qualified Common.Utils             as U
import qualified Collateral
import Plutus.Model.V2
import qualified Plutus.Model.V2 as V2 (mkTypedValidator)

data RequestDatum = RequestDatum
    { borrowersNftTn        :: TokenName
    , borrowersAddress      :: Address
    , loan                  :: AssetClass
    , loanAmnt              :: Integer
    , interest              :: AssetClass
    , interestAmnt          :: Integer
    , collateral            :: AssetClass
    , collateralAmnt        :: Integer
    , loanDuration          :: POSIXTime
    , liquidateNft          :: CurrencySymbol
    , collateralFactor      :: Integer   -- Colalteral factor used for liquidation
    , liquidationCommission :: Integer   -- How much % borrower will pay for lender when liquidated (before time passes)
    , requestExpiration     :: POSIXTime
    , lenderNftTn           :: TokenName
    , lendDate              :: POSIXTime
    } deriving (Show, Generic)

PlutusTx.unstableMakeIsData ''RequestDatum -- [('RequestDatum, 0)]
PlutusTx.makeLift ''RequestDatum

data ContractInfo = ContractInfo
    { lenderNftCs    :: CurrencySymbol
    , borrowersNftCs :: CurrencySymbol
    , collateralSc   :: Address
    } deriving (Show, Generic)

PlutusTx.makeLift ''ContractInfo

{-# INLINABLE mkValidator #-}
mkValidator :: ContractInfo -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidator
  contractInfo@ContractInfo{..}
  (unsafeFromBuiltinData -> dat :: RequestDatum)
  (unsafeFromBuiltinData -> lenderTn :: TokenName)
  (unsafeFromBuiltinData -> ctx :: ScriptContext)
  = check validate
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
      Just ub -> case txOutDatum txo of
        OutputDatumHash dh -> findDatumHash' (expectedNewDatum ub) (U.info ctx) == Just dh
        _                  -> False
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
    validate = True
      -- validateTxOuts &&
      -- validateMint &&
      -- borrowerGetsWhatHeWants &&
      -- txHasOneRequestInputOnly &&
      -- txHasOneScInputOnly &&
      -- validateExpiration ||
      -- validateBorrowerMint

request :: ContractInfo -> Script
request contractInfo = fromCompiledCode $
    ($$(PlutusTx.compile [|| mkValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode contractInfo)

-- code below is for plutus-simple-model testing
type RequestSc = TypedValidator RequestDatum TokenName

requestValidatorV2 :: ContractInfo -> RequestSc
requestValidatorV2 info = V2.mkTypedValidator
  ($$(PlutusTx.compile [|| mkValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode info)