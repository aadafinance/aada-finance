{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Interest
  ( interest
  , interestShortBs
  , validator
  , typedValidator
  , interestAddress
  , ContractInfo(..)
  , InterestDatum(..)
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
import           Prelude              (Show (..))
import           Ledger.Typed.Scripts as Scripts
import           Ledger
import qualified Common.Utils             as U
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

data ContractInfo = ContractInfo
    { lenderNftCs  :: !CurrencySymbol
    } deriving (Show, Generic, ToJSON, FromJSON)

data InterestDatum = InterestDatum
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

{-# INLINABLE mkValidator #-}
mkValidator :: ContractInfo -> InterestDatum -> Integer -> ScriptContext -> Bool
mkValidator contractInfo@ContractInfo{..} dat _ ctx = validate
  where
    validate :: Bool
    validate = case U.mintFlattened ctx of
      [(cs, tn, amt)] -> (amt == (-1)) &&
                         cs == lenderNftCs &&
                         tn == lenderNftTn dat
      _               -> False

data Interest
instance Scripts.ValidatorTypes Interest where
    type instance DatumType Interest = InterestDatum
    type instance RedeemerType Interest = Integer

typedValidator :: ContractInfo -> Scripts.TypedValidator Interest
typedValidator contractInfo = Scripts.mkTypedValidator @Interest
    ($$(PlutusTx.compile [|| mkValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode contractInfo)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @InterestDatum @Integer


validator :: ContractInfo -> Validator
validator = Scripts.validatorScript . typedValidator

script :: ContractInfo -> Plutus.Script
script = Plutus.unValidatorScript . validator

interestShortBs :: ContractInfo -> SBS.ShortByteString
interestShortBs = SBS.toShort . LBS.toStrict . serialise . script

interest :: ContractInfo -> PlutusScript PlutusScriptV1
interest = PlutusScriptSerialised . interestShortBs

interestAddress :: ContractInfo -> Address
interestAddress = scriptHashAddress . Scripts.validatorHash . typedValidator

PlutusTx.makeIsDataIndexed ''InterestDatum [('InterestDatum, 0)]
PlutusTx.makeIsDataIndexed ''ContractInfo [('ContractInfo, 1)]
PlutusTx.makeLift ''ContractInfo
