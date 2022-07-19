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

data RequestDatum = RequestDatum
    { borrowersNFT      :: !CurrencySymbol
    , borrowersPkh      :: !PubKeyHash
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
    } deriving (Show, Generic)

PlutusTx.makeIsDataIndexed ''RequestDatum [('RequestDatum, 0)]
PlutusTx.makeLift ''RequestDatum

data ContractInfo = ContractInfo
    { borrower       :: !TokenName
    , lender         :: !TokenName
    , collateralcsvh :: !ValidatorHash
    , timeNft        :: !CurrencySymbol
    } deriving (Show, Generic)

PlutusTx.makeLift ''ContractInfo

{-# INLINABLE mkValidator #-}
mkValidator :: ContractInfo -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidator
  contractInfo@ContractInfo{..}
  (unsafeFromBuiltinData -> dat :: RequestDatum)
  _
  (unsafeFromBuiltinData -> ctx :: ScriptContext)
  = check validate
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    ownInput :: Maybe TxOut
    ownInput = case findOwnInput ctx of
      Just txin -> Just $ txInInfoResolved txin
      Nothing   -> Nothing

    ownValue :: Maybe Value
    ownValue = case ownInput of
      Just txo -> Just $ txOutValue txo
      Nothing  -> Nothing

    valueToCollateralSc :: Value
    valueToCollateralSc = foldr (\(_, y) acc -> y <> acc) (PlutusTx.Prelude.mempty :: Value) (scriptOutputsAt collateralcsvh info)

    containsAmount :: (CurrencySymbol, TokenName, Integer) -> Bool
    containsAmount (cs, tn, n) = valueOf valueToCollateralSc cs tn >= n

    validateCollateral :: Bool
    validateCollateral = case ownValue of
      Just v  -> foldr (\x acc -> containsAmount x && acc) True (flattenValue v)
      Nothing -> False

    valueToBorrower :: Value
    valueToBorrower = valuePaidTo info (borrowersPkh dat)

    borrowerGetsWhatHeWants :: Bool
    borrowerGetsWhatHeWants = valueOf valueToBorrower (loancs dat) (loantn dat) >= loanamnt dat

    validateTimeNftIsSentToCollateralSc :: Bool
    validateTimeNftIsSentToCollateralSc = elem 1 $ (\(_, tn, _) -> valueOf valueToCollateralSc timeNft tn) <$> flattenValue valueToCollateralSc

    filterOutTimeNft :: (CurrencySymbol, TokenName, Integer) -> Bool
    filterOutTimeNft (cs, _, _) = cs /= timeNft

    mintFlattened :: [(CurrencySymbol, TokenName, Integer)]
    mintFlattened = filter filterOutTimeNft $ flattenValue $ txInfoMint info

    validateMint :: Bool
    validateMint = case mintFlattened of
      [(cs, tn, amt)] -> (amt == 2) &&
                         (tn == lender) &&
                         (valueOf valueToCollateralSc cs lender == 1)
      _               -> False

    validateBorrowerMint :: Bool
    validateBorrowerMint = case mintFlattened of
      [(cs, tn, amt)] -> (amt == (-1)) &&
                         (tn == borrower && cs == borrowersNFT dat)
      _               -> False


    mapMaybe :: (a -> Maybe b) -> [a] -> [b]
    mapMaybe _ [] = []
    mapMaybe f (x:xs) =
      let rs = mapMaybe f xs in
      case f x of
        Nothing -> rs
        Just r  -> r:rs

    getCollateralScHashes :: [DatumHash]
    getCollateralScHashes = mapMaybe (\case OutputDatumHash h -> Just h ; _ -> Nothing) $ fst <$> (scriptOutputsAt collateralcsvh info)

    validateOutputHash :: DatumHash -> Bool
    validateOutputHash h = h `elem` getCollateralScHashes

    ownInputHash :: Bool
    ownInputHash = case ownInput of
      Just (TxOut {txOutDatum = OutputDatumHash h}) -> validateOutputHash h
      Nothing   -> False

    range :: POSIXTimeRange
    range = txInfoValidRange info

    validateExpiration :: Bool
    validateExpiration = after (requestExpiration dat) range

    validate :: Bool
    validate = traceIfFalse "datum hash validation fail" ownInputHash &&
               traceIfFalse "2 lender tokens wasn't minted and or 1 of them wasn't sent to collateral sc" validateMint &&
               traceIfFalse "borrower didn't receive the loan" borrowerGetsWhatHeWants &&
               traceIfFalse "time nft not sent to collateral sc" validateTimeNftIsSentToCollateralSc &&
               traceIfFalse "collateral not sent to collateral sc" validateCollateral &&
               traceIfFalse "Loan request has expired or txValidTo wasn't set correctly" validateExpiration ||
               (traceIfFalse "borrower nft wasn't burnt" validateBorrowerMint)

request :: ContractInfo -> Script
request contractInfo = fromCompiledCode $
    ($$(PlutusTx.compile [|| mkValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode contractInfo)
