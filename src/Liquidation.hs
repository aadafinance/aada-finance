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
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Liquidation
  ( liquidation
  , ContractInfo(..)
  ) where

-- import           Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV1)

import           Codec.Serialise
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS

import           Plutus.V2.Ledger.Contexts hiding (TxOut(..))
import qualified Plutus.V1.Ledger.Scripts as Plutus
import           Plutus.V1.Ledger.Scripts
import           Plutus.V1.Ledger.Value
import qualified PlutusTx
import           PlutusTx.Prelude hiding (Semigroup (..), unless)
import qualified Common.Utils             as U
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import           Prelude              (Show (..))
import           PlutusTx (unsafeFromBuiltinData)

data ContractInfo = ContractInfo
    { borrowerNftCs  :: CurrencySymbol
    } deriving (Show, Generic)

PlutusTx.makeIsDataIndexed ''ContractInfo [('ContractInfo, 1)]
PlutusTx.makeLift ''ContractInfo

{-# INLINABLE mkValidator #-}
mkValidator :: ContractInfo -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidator
  contractInfo@ContractInfo{..}
    (unsafeFromBuiltinData -> borrowerNftTn :: TokenName)
    (unsafeFromBuiltinData -> rdm :: Integer)
    (unsafeFromBuiltinData -> ctx :: ScriptContext)
    = check $ rdm == 65

liquidation :: ContractInfo -> Script
liquidation contractInfo = fromCompiledCode $
    ($$(PlutusTx.compile [|| mkValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode contractInfo)

-- {-# INLINABLE mkValidator #-}
-- mkValidator :: ContractInfo -> BuiltinData -> BuiltinData -> BuiltinData -> ()
-- mkValidator
--   contractInfo@ContractInfo{..}
--     (unsafeFromBuiltinData -> borrowerNftTn :: TokenName)
--     _
--     (unsafeFromBuiltinData -> ctx :: ScriptContext)
--     = check $ case U.mintFlattened ctx of
--       [(cs, tn, amt)] -> (amt == (-1)) &&
--                          (tn == borrowerNftTn) &&
--                          (cs == borrowerNftCs)
--       _               -> False

-- liquidation :: ContractInfo -> Script
-- liquidation contractInfo = fromCompiledCode $
--     ($$(PlutusTx.compile [|| mkValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode contractInfo)
