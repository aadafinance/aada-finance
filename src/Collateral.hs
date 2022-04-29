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
  , validator
  ) where

import           Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV1)

import           Codec.Serialise
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS

import           Plutus.V1.Ledger.Contexts
import qualified Plutus.V1.Ledger.Scripts as Plutus
import           Plutus.V1.Ledger.Scripts
import           Plutus.V1.Ledger.Value
import qualified PlutusTx
import           PlutusTx.Prelude hiding (Semigroup (..), unless)
import           Prelude              (Semigroup (..), Show (..))
import           PlutusTx.Builtins.Internal as B

import           Ledger.Typed.Scripts as Scripts
import           Ledger hiding (singleton)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

data CollateralDatum = CollateralDatum
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
    } deriving Show

data ContractInfo = ContractInfo
    { borrower     :: !TokenName
    , lender       :: !TokenName
    , interestscvh :: !ValidatorHash
    , timeNft      :: !CurrencySymbol
    } deriving (Show, Generic, ToJSON, FromJSON)

{-# INLINEABLE intToByteString #-}
intToByteString :: Integer -> BuiltinByteString
intToByteString x = if x `divideInteger` 10 == 0 then digitToByteString x
  else
    B.appendByteString (intToByteString (x `divideInteger` 10)) (digitToByteString (x `B.modInteger` 10))
       where
         digitToByteString :: Integer -> BuiltinByteString
         digitToByteString d = B.consByteString (d `addInteger` asciZero) B.emptyByteString

         asciZero :: Integer
         asciZero = 48

{-# INLINABLE mkValidator #-}
mkValidator :: ContractInfo -> CollateralDatum -> POSIXTime -> ScriptContext -> Bool
mkValidator contractInfo@ContractInfo{..} dat mintdate ctx = traceIfFalse "Collateral failed" validate
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    mintFlattened :: [(CurrencySymbol, TokenName, Integer)]
    mintFlattened = flattenValue $ txInfoMint info

    getLoanAmnt :: Value -> Integer
    getLoanAmnt v = valueOf v (loancs dat) (loantn dat)

    getInterestAmnt :: Value -> Integer
    getInterestAmnt v = valueOf v (interestcs dat) (interesttn dat)

    valueToInterestCs :: Value
    valueToInterestCs = foldr (\(_, y) acc -> y <> acc) (PlutusTx.Prelude.mempty :: Value) (scriptOutputsAt interestscvh info)

    ownInput :: Maybe TxOut
    ownInput = case findOwnInput ctx of
      Just txin -> Just $ txInInfoResolved txin
      Nothing   -> Nothing

    ownValue :: Maybe Value
    ownValue = case ownInput of
      Just txo -> Just $ txOutValue txo
      Nothing  -> Nothing

    validateDebtAmnt :: Bool
    validateDebtAmnt = getLoanAmnt valueToInterestCs >= loanamnt dat

    validateInterestAmnt :: Bool
    validateInterestAmnt = getInterestAmnt valueToInterestCs >= interestamnt dat

    validateDebtAndInterestAmnt :: Bool
    validateDebtAndInterestAmnt =
      not ((interestcs dat == loancs dat) && (interesttn dat == loantn dat)) || (getInterestAmnt valueToInterestCs + getLoanAmnt valueToInterestCs >= loanamnt dat + interestamnt dat)

    lenderNftFilter :: (CurrencySymbol, TokenName, Integer) -> Bool
    lenderNftFilter (cs, tn, n) = tn == lender && n == 1 && cs /= collateralcs dat

    filterValues :: Value -> [(CurrencySymbol, TokenName, Integer)]
    filterValues v = filter lenderNftFilter $ flattenValue v

    containsFlattenedValue :: [(CurrencySymbol, TokenName, Integer)] -> (CurrencySymbol, TokenName, Integer) -> Bool
    containsFlattenedValue xs (cs, tn, _) = foldr (\(cs', tn', _) acc -> (cs' == cs && tn' == tn) || acc) False xs

    validateNftIsPassedOn :: Bool
    validateNftIsPassedOn = case ownValue of
      Just v  -> foldr (\x acc -> containsFlattenedValue (filterValues v) x && acc) True (filterValues valueToInterestCs)
      Nothing -> False

    validateBorrowerNftBurn :: Bool
    validateBorrowerNftBurn = any (\(cs, tn, n) -> cs == borrowersNFT dat && tn == borrower && n == (-1)) mintFlattened    

    validateBorrower :: Bool
    validateBorrower = validateDebtAmnt &&
                       validateInterestAmnt &&
                       validateDebtAndInterestAmnt &&
                       validateNftIsPassedOn &&
                       validateBorrowerNftBurn

    range :: POSIXTimeRange
    range = txInfoValidRange info

    checkDeadline :: Bool
    checkDeadline = contains (from (mintdate + repayinterval dat)) range

    tokenNameIsCorrect :: TokenName -> Bool
    tokenNameIsCorrect tn = fromBuiltin $ equalsByteString (unTokenName tn) (intToByteString $ getPOSIXTime mintdate)

    getTimeTokenName :: Maybe TokenName
    getTimeTokenName = (\(_, tn, _) -> tn) <$> find (\(cs, _, n) -> cs == timeNft && n == (-1)) mintFlattened

    checkMintTnName :: Bool
    checkMintTnName = maybe False tokenNameIsCorrect getTimeTokenName

    validateLender :: Bool
    validateLender = checkDeadline &&
                     checkMintTnName

    validate :: Bool
    validate = validateLender || validateBorrower

data Collateral
instance Scripts.ValidatorTypes Collateral where
    type instance DatumType Collateral = CollateralDatum
    type instance RedeemerType Collateral = POSIXTime

typedValidator :: ContractInfo -> Scripts.TypedValidator Collateral
typedValidator contractInfo = Scripts.mkTypedValidator @Collateral
    ($$(PlutusTx.compile [|| mkValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode contractInfo)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @CollateralDatum @POSIXTime

validator :: ContractInfo -> Validator
validator = Scripts.validatorScript . typedValidator

PlutusTx.makeIsDataIndexed ''CollateralDatum [('CollateralDatum, 0)]
PlutusTx.makeIsDataIndexed ''ContractInfo [('ContractInfo, 1)]
PlutusTx.makeLift ''ContractInfo

script :: ContractInfo -> Plutus.Script
script = Plutus.unValidatorScript . validator

scriptAsCbor :: ContractInfo -> LBS.ByteString
scriptAsCbor = serialise . validator

collateral :: ContractInfo -> PlutusScript PlutusScriptV1
collateral = PlutusScriptSerialised . collateralShortBs

collateralShortBs :: ContractInfo -> SBS.ShortByteString
collateralShortBs = SBS.toShort . LBS.toStrict . scriptAsCbor
