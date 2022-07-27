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
import           Prelude              (Semigroup (..), Show (..))
import qualified PlutusTx.Builtins.Internal as B

import           Ledger.Typed.Scripts as Scripts
import           Ledger hiding (singleton)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import PlutusTx.Builtins (equalsByteString, divideInteger, addInteger, multiplyInteger)
import qualified Common.Utils             as U

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
    , liquidateNft      :: !CurrencySymbol
    , collateraltn          :: !TokenName -- collateral token name
    , collateralamnt        :: !Integer   -- amount of collateral
    , collateralFactor      :: !Integer   -- Colalteral factor used for liquidation
    , liquidationCommission :: !Integer   -- How much % borrower will pay for lender when liquidated (before time passes)
    , requestExpiration     :: !POSIXTime
    } deriving (Show, Generic, ToJSON, FromJSON)

data CollateralRedeemer = CollateralRedeemer
  { mintdate        :: !POSIXTime
  , interestPayDate :: !POSIXTime
  } deriving (Show, Generic, ToJSON, FromJSON)

data ContractInfo = ContractInfo
    { borrower     :: !TokenName
    , lender       :: !TokenName
    , interestscvh :: !ValidatorHash
    , timeNft      :: !CurrencySymbol
    } deriving (Show, Generic, ToJSON, FromJSON)

{-# INLINABLE mkValidator #-}
mkValidator :: ContractInfo -> CollateralDatum -> CollateralRedeemer -> ScriptContext -> Bool
mkValidator contractInfo@ContractInfo{..} dat rdm ctx = validate
  where
    mintFlattened :: [(CurrencySymbol, TokenName, Integer)]
    mintFlattened = flattenValue $ txInfoMint (U.info ctx)

    getLoanAmnt :: Value -> Integer
    getLoanAmnt v = valueOf v (loancs dat) (loantn dat)

    getInterestAmnt :: Value -> Integer
    getInterestAmnt v = valueOf v (interestcs dat) (interesttn dat)

    -- TODO What if interest is divided into different utxos?
    valueToInterestSc :: Value
    valueToInterestSc = foldr (\(_, y) acc -> y <> acc) (PlutusTx.Prelude.mempty :: Value) (scriptOutputsAt interestscvh (U.info ctx))

    ownValue :: Maybe Value
    ownValue = do
      i <- findOwnInput ctx
      txo <- Just $ txInInfoResolved i
      pure $ txOutValue txo

    validateDebtAmnt :: Bool
    validateDebtAmnt = getLoanAmnt valueToInterestSc >= loanamnt dat

    interestPercentage :: Integer
    interestPercentage = case (mintdate rdm + repayinterval dat) < interestPayDate rdm of
      True  -> 100
      False -> (getPOSIXTime (repayinterval dat) `multiplyInteger` 100) `divideInteger` getPOSIXTime loanHeld
       where
         loanHeld = interestPayDate rdm - mintdate rdm

    validateInterestAmnt :: Bool
    validateInterestAmnt = getInterestAmnt valueToInterestSc >= ((interestamnt dat `multiplyInteger` 100) `divideInteger` interestPercentage)

    validateDebtAndInterestAmnt :: Bool
    validateDebtAndInterestAmnt = not ((interestcs dat == loancs dat) && (interesttn dat == loantn dat)) || (getLoanAmnt valueToInterestSc >= loanamnt dat + interestamnt dat)

    lenderNftFilter :: (CurrencySymbol, TokenName, Integer) -> Bool
    lenderNftFilter (cs, tn, n) = tn == lender && n == 1 && cs /= collateralcs dat

    filterValues :: Value -> [(CurrencySymbol, TokenName, Integer)]
    filterValues v = filter lenderNftFilter $ flattenValue v

    containsFlattenedValue :: [(CurrencySymbol, TokenName, Integer)] -> (CurrencySymbol, TokenName, Integer) -> Bool
    containsFlattenedValue xs (cs, tn, _) = foldr (\(cs', tn', _) acc -> (cs' == cs && tn' == tn) || acc) False xs

    validateNftIsPassedOn :: Bool
    validateNftIsPassedOn = case ownValue of
      Just v  -> foldr (\x acc -> containsFlattenedValue (filterValues v) x && acc) True (filterValues valueToInterestSc)
      Nothing -> False

    validateBorrowerNftBurn :: Bool
    validateBorrowerNftBurn = any (\(cs, tn, n) -> cs == borrowersNFT dat && tn == borrower && n == (-1)) mintFlattened

    validateBorrower :: Bool
    validateBorrower = traceIfFalse "invalid debt amount sent to interest sc" validateDebtAmnt &&
                       traceIfFalse "invalid interest amount sent to interest sc" validateInterestAmnt &&
                       traceIfFalse "invalid debt and interest amount" validateDebtAndInterestAmnt &&
                       traceIfFalse "Lender nft is not passed on to interest sc" validateNftIsPassedOn &&
                       traceIfFalse "borrower nft is not burnt" validateBorrowerNftBurn &&
                       traceIfFalse "borrower deadline check fail" checkBorrowerDeadLine &&
                       traceIfFalse "invalid time nft token name" checkMintTnName

    checkDeadline :: Bool
    checkDeadline = traceIfFalse "deadline check fail" (contains (from (mintdate rdm + repayinterval dat)) (U.range ctx))

    checkBorrowerDeadLine :: Bool
    checkBorrowerDeadLine = traceIfFalse "borrower deadline check fail" (contains (U.range ctx) (from (interestPayDate rdm)))

    tokenNameIsCorrect :: TokenName -> Bool
    tokenNameIsCorrect tn = equalsByteString (unTokenName tn) (U.intToByteString $ getPOSIXTime (mintdate rdm))

    getTimeTokenName :: Maybe TokenName
    getTimeTokenName = case ownValue of
      Just v -> (\(_, tn, _) -> tn) <$> find (\(cs, _, n) -> cs == timeNft && n == 1) (flattenValue v)
      Nothing -> Nothing

    checkMintTnName :: Bool
    checkMintTnName = traceIfFalse "invalid time token name" (maybe False tokenNameIsCorrect getTimeTokenName)

    inputHasBurntLNft :: CurrencySymbol -> Bool
    inputHasBurntLNft cs = case ownValue of
      Just v  -> valueOf v cs lender == 1
      Nothing -> False

    checkLNftsAreBurnt :: Bool
    checkLNftsAreBurnt = traceIfFalse "2 Lender Nfts not burnt" (any (\(cs, _, n) -> inputHasBurntLNft cs && n == (-2)) mintFlattened)

    checkForLiquidationNft :: Bool
    checkForLiquidationNft = traceIfFalse "liqudation token was not found" (any (\(cs, _, _) -> cs == liquidateNft dat) mintFlattened)

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