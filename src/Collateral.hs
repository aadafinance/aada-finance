{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use guards" #-}

module Collateral
    ( collateralScript
    , ContractInfo(..)
    , CollateralDatum(..)
    , collateralAddress
    ) where


import           Codec.Serialise
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS

import           Plutus.V2.Ledger.Contexts hiding (TxOut(..))
import           Plutus.V1.Ledger.Scripts
import           Plutus.V1.Ledger.Value
import           Plutus.V1.Ledger.Interval
import           Plutus.V1.Ledger.Address hiding (toValidatorHash)
import           Plutus.V1.Ledger.Time
import           Plutus.V2.Ledger.Tx
import           Plutus.V2.Ledger.Api hiding (TxOut(..))
import qualified PlutusTx
import           PlutusTx.Prelude
import           PlutusTx (unsafeFromBuiltinData)
import           PlutusPrelude hiding (Semigroup (..), unless)
import           Prelude              (Show (..))

import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import PlutusTx.Builtins.Internal (divideInteger, multiplyInteger)
import qualified Common.Utils             as U
import Plutus.Model.V2
-- import Plutus.V1.Ledger.Api

data CollateralDatum = CollateralDatum
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


data ContractInfo = ContractInfo
    { lenderNftCs    :: CurrencySymbol
    , borrowersNftCs :: CurrencySymbol
    , interestSc     :: Address
    } deriving (Show, Generic)

{-# INLINABLE mkValidator #-}
mkValidator :: ContractInfo -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidator
  contractInfo@ContractInfo{..}
  (unsafeFromBuiltinData -> dat :: CollateralDatum)
  (unsafeFromBuiltinData -> rdm :: Integer)
  (unsafeFromBuiltinData -> ctx :: ScriptContext)
  = check validate
  where
    getLoanAmnt :: Value -> Integer
    getLoanAmnt v = assetClassValueOf v (loan dat)

    getInterestAmnt :: Value -> Integer
    getInterestAmnt v = assetClassValueOf v (interest dat)

    validateDebtAmnt :: TxOut -> Bool
    validateDebtAmnt txo = getLoanAmnt (txOutValue txo) >= loanAmnt dat

    getPartialInterest :: POSIXTime -> Integer
    getPartialInterest interestPayDate = if repayIntEnd > interestPayDate && denominator > 0
      then
        (loanHeld `multiplyInteger` interestAmnt dat) `divideInteger` denominator
      else
        interestAmnt dat
      where
        denominator = getPOSIXTime (loanDuration dat)
        repayIntEnd = lendDate dat + loanDuration dat
        loanHeld = getPOSIXTime $ interestPayDate - lendDate dat

    validateInterestAmnt :: TxOut -> Bool
    validateInterestAmnt txo = case U.getUpperBound ctx of
      Just interestPayDate -> getInterestAmnt (txOutValue txo) >= getPartialInterest interestPayDate
      Nothing              -> False

    validateDebtAndInterestAmnt :: TxOut -> Bool
    validateDebtAndInterestAmnt txo = case U.getUpperBound ctx of
      Just interestPayDate -> interest dat /= loan dat ||
                              (getLoanAmnt (txOutValue txo) >= loanAmnt dat + getPartialInterest interestPayDate)
      Nothing              -> False

    validateBorrowerNftBurn :: Bool
    validateBorrowerNftBurn = any
      (\(cs, tn, n) -> cs == borrowersNftCs && tn == borrowersNftTn dat && n == (-1))
      (U.mintFlattened ctx)

    findDatumHash' :: ToData a => a -> TxInfo -> Maybe DatumHash
    findDatumHash' datum info = findDatumHash (Datum $ toBuiltinData datum) info

    containsNewDatum :: TxOut -> Bool
    containsNewDatum txo = case txOutDatum txo of
      OutputDatumHash dh -> findDatumHash' (lenderNftTn dat) (U.info ctx) == Just dh
      _                  -> False

      -- findDatumHash' (lenderNftTn dat) (U.info ctx) == txOutDatum txo

    destinationIsToInterestSc :: TxOut -> Bool
    destinationIsToInterestSc txo = txOutAddress txo == interestSc

    txOutValidate :: TxOut -> Bool
    txOutValidate txo = containsNewDatum txo &&
                        destinationIsToInterestSc txo &&
                        validateDebtAmnt txo &&
                        validateInterestAmnt txo &&
                        validateDebtAndInterestAmnt txo &&
                        checkForTokensDos txo

    validateTxOuts :: Bool
    validateTxOuts = any txOutValidate (txInfoOutputs $ U.info ctx)

    checkForTokensDos :: TxOut -> Bool
    checkForTokensDos txo = length ((flattenValue . txOutValue) txo) <= 3

    validateReturn :: Bool
    validateReturn = validateBorrowerNftBurn && validateTxOuts

    checkDeadline :: Bool
    checkDeadline =
      contains (from (lendDate dat + loanDuration dat)) (U.range ctx)

    checkLNftIsBurnt :: Bool
    checkLNftIsBurnt =
      valueOf (txInfoMint $ U.info ctx) lenderNftCs (lenderNftTn dat) == (-1)

    checkForLiquidationToken :: Bool
    checkForLiquidationToken = case filtered of
        [] -> False
        xs -> all (\(_, _, n) -> n > 0) xs
      where
        filtered = filter (\(cs, _tn, _n) -> cs == liquidateNft dat) (U.mintFlattened ctx)

    validateLiquidation :: Bool
    validateLiquidation = checkLNftIsBurnt && (checkDeadline || checkForLiquidationToken)

    validate :: Bool
    validate = validateLiquidation || validateReturn

collateralScript :: ContractInfo -> Script
collateralScript contractInfo = fromCompiledCode $
    ($$(PlutusTx.compile [|| mkValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode contractInfo)

PlutusTx.makeIsDataIndexed ''CollateralDatum [('CollateralDatum, 0)]
PlutusTx.makeIsDataIndexed ''ContractInfo [('ContractInfo, 1)]
PlutusTx.makeLift ''ContractInfo

-- code below is for plutus-simple-model testing
type CollateralSc = TypedValidator CollateralDatum Integer

collateralValidatorV2 :: ContractInfo -> CollateralSc
collateralValidatorV2 info = TypedValidator $ toV2 $ mkValidatorScript
  ($$(PlutusTx.compile [|| mkValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode info)

collateralAddress :: ContractInfo -> Maybe StakingCredential -> Address
collateralAddress info stakingCred = Address (ScriptCredential $ validatorHash (collateralValidatorV2 info)) stakingCred
