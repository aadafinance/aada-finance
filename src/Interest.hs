{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Interest
  ( interest
  , ContractInfo(..)
  , interestAddress
  ) where

import           Codec.Serialise
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS

import           Plutus.V2.Ledger.Contexts
import           Plutus.V2.Ledger.Api (StakingCredential(..), Credential(..))
import qualified Plutus.V1.Ledger.Scripts as Plutus
import           Plutus.V1.Ledger.Scripts
import           Plutus.V1.Ledger.Value
import           Plutus.V1.Ledger.Address
import qualified PlutusTx
import           PlutusTx (unsafeFromBuiltinData)
import           PlutusTx.Prelude hiding (Semigroup (..), unless)
import           Prelude              (Show (..))
import qualified Common.Utils             as U
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import Plutus.Model.V2

data ContractInfo = ContractInfo
    { lenderNftCs  :: CurrencySymbol
    } deriving (Show, Generic)

PlutusTx.makeIsDataIndexed ''ContractInfo [('ContractInfo, 1)]
PlutusTx.makeLift ''ContractInfo

{-# INLINABLE mkValidator #-}
mkValidator :: ContractInfo -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidator
  contractInfo@ContractInfo{..}
  (unsafeFromBuiltinData -> lenderNftTn :: TokenName)
  _
  (unsafeFromBuiltinData -> ctx :: ScriptContext)
  = check $ case U.mintFlattened ctx of
    [(cs, tn, amt)] -> (amt == (-1)) &&
                        cs == lenderNftCs &&
                        tn == lenderNftTn
    _               -> False

interest :: ContractInfo -> Script
interest contractInfo = fromCompiledCode $
    ($$(PlutusTx.compile [|| mkValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode contractInfo)

-- code below is for plutus-simple-model testing
type InterestSc = TypedValidator TokenName BuiltinData

interestValidatorV2 :: ContractInfo -> InterestSc
interestValidatorV2 info = TypedValidator $ toV2 $ mkValidatorScript
  ($$(PlutusTx.compile [|| mkValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode info)

interestAddress :: ContractInfo -> Maybe StakingCredential -> Address
interestAddress info stakingCred = Address (ScriptCredential $ validatorHash (interestValidatorV2 info)) stakingCred

-- code below is for Ply-core
-- getPlyTypedScriptV2 :: ContractInfo -> TypedScript ScriptVersion Script
-- getPlyTypedScriptV2 info = TypedScript ScriptV2 $ interest info
