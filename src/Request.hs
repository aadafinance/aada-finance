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

data RequestDatum = RequestDatum
    { borrowersNFT      :: !CurrencySymbol
    , borrowersPkh      :: !PaymentPubKeyHash
    , loantn            :: !TokenName
    , loancs            :: !CurrencySymbol
    , loanamnt          :: !Integer
    , interesttn        :: !TokenName
    , interestcs        :: !CurrencySymbol
    , interestamnt      :: !Integer
    , collateralcs      :: !CurrencySymbol
    , repayinterval     :: !POSIXTime
    , liquidateNft      :: !CurrencySymbol
    , collateraltn          :: !TokenName -- collateral token name
    , collateralamnt        :: !Integer   -- amount of collateral
    , collateralFactor      :: !Integer   -- Colalteral factor used for liquidation
    , liquidationCommission :: !Integer   -- How much % borrower will pay for lender when liquidated (before time passes)
    , requestExpiration     :: !POSIXTime
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
mkValidator :: ContractInfo -> RequestDatum -> Integer -> ScriptContext -> Bool
mkValidator contractInfo@ContractInfo{..} dat _ ctx = validate
  where
    containsAmount :: (CurrencySymbol, TokenName, Integer) -> Bool
    containsAmount (cs, tn, n) = valueOf (U.valueToSc collateralcsvh ctx) cs tn >= n

    validateCollateral :: Bool
    validateCollateral = case U.ownValue ctx of
      Just v  -> foldr (\x acc -> containsAmount x && acc) True (flattenValue v)
      Nothing -> False

    valueToBorrower :: Value
    valueToBorrower = valuePaidTo (U.info ctx) (unPaymentPubKeyHash $ borrowersPkh dat)

    borrowerGetsWhatHeWants :: Bool
    borrowerGetsWhatHeWants = valueOf valueToBorrower (loancs dat) (loantn dat) >= loanamnt dat

    validateTimeNftIsSentToCollateralSc :: Bool
    validateTimeNftIsSentToCollateralSc = elem 1 $ (\(_, tn, _) -> valueOf (U.valueToSc collateralcsvh ctx) timeNft tn) <$> flattenValue (U.valueToSc collateralcsvh ctx)

    filterOutTimeNft :: (CurrencySymbol, TokenName, Integer) -> Bool
    filterOutTimeNft (cs, _, _) = cs /= timeNft

    mintFlattened :: [(CurrencySymbol, TokenName, Integer)]
    mintFlattened = filter filterOutTimeNft $ flattenValue $ txInfoMint (U.info ctx)

    validateMint :: Bool
    validateMint = case mintFlattened of
      [(cs, tn, amt)] -> (amt == 2) &&
                         (cs == lenderNftCs) &&
                         (valueOf (U.valueToSc collateralcsvh ctx) cs tn == 1)
      _               -> False

    validateBorrowerMint :: Bool
    validateBorrowerMint = case mintFlattened of
      [(cs, tn, amt)] -> (amt == (-1)) &&
                         (tn == borrower && cs == borrowersNFT dat)
      _               -> False

    getCollateralScHashes :: [DatumHash]
    getCollateralScHashes = map fst (scriptOutputsAt collateralcsvh (U.info ctx))

    validateOutputHash :: DatumHash -> Bool
    validateOutputHash h = h `elem` getCollateralScHashes

    ownInputHash :: Bool
    ownInputHash = case U.ownInput ctx of
      Just txin -> maybe False validateOutputHash (txOutDatumHash txin)
      Nothing   -> False

    validateExpiration :: Bool
    validateExpiration = after (requestExpiration dat) (U.range ctx)

    validate :: Bool
    validate = traceIfFalse "datum hash validation fail" ownInputHash &&
               traceIfFalse "2 lender tokens wasn't minted and or 1 of them wasn't sent to collateral sc" validateMint &&
               traceIfFalse "borrower didn't receive the loan" borrowerGetsWhatHeWants &&
               traceIfFalse "time nft not sent to collateral sc" validateTimeNftIsSentToCollateralSc &&
               traceIfFalse "collateral not sent to collateral sc" validateCollateral &&
               traceIfFalse "Loan request has expired or txValidTo wasn't set correctly" validateExpiration ||
               traceIfFalse "borrower nft wasn't burnt" validateBorrowerMint

data RequestDataTypes
instance Scripts.ValidatorTypes RequestDataTypes where
    type instance DatumType    RequestDataTypes = RequestDatum
    type instance RedeemerType RequestDataTypes = Integer

requestTypedValidator :: ContractInfo -> Scripts.TypedValidator RequestDataTypes
requestTypedValidator contractInfo = Scripts.mkTypedValidator @RequestDataTypes
    ($$(PlutusTx.compile [|| mkValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode contractInfo)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @RequestDatum @Integer

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
