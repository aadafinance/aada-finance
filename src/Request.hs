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

PlutusTx.makeIsDataIndexed ''RequestDatum [('RequestDatum, 0)]
PlutusTx.makeLift ''RequestDatum

data ContractInfo = ContractInfo
    { borrower       :: !TokenName
    , lenderNftCs    :: !CurrencySymbol
    , collateralcsvh :: !ValidatorHash
    , timeNft        :: !CurrencySymbol
    } deriving (Show, Generic, ToJSON, FromJSON)

{-# INLINABLE mkValidator #-}
mkValidator :: ContractInfo -> RequestDatum -> TokenName -> ScriptContext -> Bool
mkValidator contractInfo@ContractInfo{..} dat lenderTn ctx = validate
  where
    valueToBorrower :: Value
    valueToBorrower = valuePaidTo (U.info ctx) (unPaymentPubKeyHash $ borrowersPkh dat)

    borrowerGetsWhatHeWants :: Bool
    borrowerGetsWhatHeWants = valueOf valueToBorrower (loancs dat) (loantn dat) >= loanamnt dat

    filterOutTimeNft :: (CurrencySymbol, TokenName, Integer) -> Bool
    filterOutTimeNft (cs, _, _) = cs /= timeNft

    mintFlattened :: [(CurrencySymbol, TokenName, Integer)]
    mintFlattened = filter filterOutTimeNft $ flattenValue $ txInfoMint (U.info ctx)

    validateMint :: Bool
    validateMint = case mintFlattened of
      [(cs, tn, amt)] -> (cs == lenderNftCs) &&
                         (tn == lenderTn) &&
                         (amt == 1)
      _               -> False

    validateBorrowerMint :: Bool
    validateBorrowerMint = case mintFlattened of
      [(cs, tn, amt)] -> (cs == borrowersNFT dat) &&
                         (tn == borrower) && 
                         (amt == (-1))
      _               -> False

    findDatumHash' :: ToData a => a -> TxInfo -> Maybe DatumHash
    findDatumHash' datum info = findDatumHash (Datum $ toBuiltinData datum) info

    expectedNewDatum :: Collateral.CollateralDatum
    expectedNewDatum = Collateral.CollateralDatum { 
        Collateral.borrowersNFT          = borrowersNFT dat 
      , Collateral.borrowersPkh          = borrowersPkh dat
      , Collateral.loantn                = loantn dat
      , Collateral.loancs                = loancs dat
      , Collateral.loanamnt              = loanamnt dat
      , Collateral.interesttn            = interesttn dat
      , Collateral.interestcs            = interestcs dat
      , Collateral.interestamnt          = interestamnt dat
      , Collateral.collateralcs          = collateralcs dat
      , Collateral.repayinterval         = repayinterval dat
      , Collateral.liquidateNft          = liquidateNft dat
      , Collateral.collateraltn          = collateraltn dat 
      , Collateral.collateralamnt        = collateralamnt dat   
      , Collateral.collateralFactor      = collateralFactor dat 
      , Collateral.liquidationCommission = liquidationCommission dat
      , Collateral.requestExpiration     = requestExpiration dat
      , Collateral.lenderNftTn           = lenderTn
    }

    validateExpiration :: Bool
    validateExpiration = after (requestExpiration dat) (U.range ctx)

    isItToCollateral :: TxOut -> Bool
    isItToCollateral txo = case toValidatorHash $ txOutAddress txo of
      Just vh -> vh == collateralcsvh
      _       -> False

    containsRequiredCollateralAmount :: TxOut -> Bool
    containsRequiredCollateralAmount txo = case U.ownValue ctx of
      Just v  -> valueOf v (collateralcs dat) (collateraltn dat) >= valueOf (txOutValue txo) (collateralcs dat) (collateraltn dat)
      Nothing -> False

    containsNewDatum :: TxOut -> Bool
    containsNewDatum txo = findDatumHash' expectedNewDatum (U.info ctx) == txOutDatumHash txo

    containtsTimeNft :: TxOut -> Bool
    containtsTimeNft txo = any (\(cs, _tn, n) -> cs == timeNft && n == 1) (flattenValue (txOutValue txo))

    txOutValidate :: TxOut -> Bool
    txOutValidate txo = isItToCollateral txo &&
                        containsRequiredCollateralAmount txo &&
                        containsNewDatum txo &&
                        containtsTimeNft txo

    validateTxOuts :: Bool
    validateTxOuts = any txOutValidate (txInfoOutputs $ U.info ctx)

    validate :: Bool
    validate = validateTxOuts &&
               traceIfFalse "lender nft was not minted" validateMint &&
               traceIfFalse "borrower didn't receive the loan" borrowerGetsWhatHeWants &&
               traceIfFalse "Loan request has expired or txValidTo wasn't set correctly" validateExpiration ||
               traceIfFalse "borrower nft wasn't burnt" validateBorrowerMint

data RequestDataTypes
instance Scripts.ValidatorTypes RequestDataTypes where
    type instance DatumType    RequestDataTypes = RequestDatum
    type instance RedeemerType RequestDataTypes = TokenName

requestTypedValidator :: ContractInfo -> Scripts.TypedValidator RequestDataTypes
requestTypedValidator contractInfo = Scripts.mkTypedValidator @RequestDataTypes
    ($$(PlutusTx.compile [|| mkValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode contractInfo)
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
