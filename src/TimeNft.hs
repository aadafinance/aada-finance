{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module TimeNft
  ( timeNft
  , intToByteString
  ) where

import           Codec.Serialise
import qualified Data.ByteString.Lazy     as LB
import qualified Data.ByteString.Short    as SBS
import qualified PlutusTx
import           PlutusTx.Prelude         hiding (Semigroup (..), unless)
import           PlutusTx.Builtins.Internal as B
import qualified Plutus.V1.Ledger.Scripts as Plutus
import           Plutus.V2.Ledger.Contexts
import           Plutus.V1.Ledger.Scripts
import           Plutus.V1.Ledger.Value
import           Plutus.V2.Ledger.Api
import           Plutus.V1.Ledger.Address
import           Plutus.V1.Ledger.Interval

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

{-# INLINABLE mkPolicy #-}
mkPolicy :: BuiltinData -> BuiltinData -> ()
mkPolicy (unsafeFromBuiltinData -> mintingdate :: POSIXTime) (unsafeFromBuiltinData -> ctx :: ScriptContext) = check validate
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    tokenNameIsCorrect :: TokenName -> Bool
    tokenNameIsCorrect tn = fromBuiltin $ equalsByteString (unTokenName tn) (intToByteString $ getPOSIXTime mintingdate)

    range :: POSIXTimeRange
    range = txInfoValidRange info

    checkDeadline :: Bool
    checkDeadline = contains (from mintingdate) range

    timeNftFilter :: (CurrencySymbol, TokenName, Integer) -> Bool
    timeNftFilter (cs, tn, _) = cs == ownCurrencySymbol ctx && tokenNameIsCorrect tn

    mintedFlattened :: [(CurrencySymbol, TokenName, Integer)]
    mintedFlattened = filter timeNftFilter $ flattenValue $ txInfoMint info

    validateMint :: (CurrencySymbol, TokenName, Integer) -> Bool
    validateMint (_, _, amnt) = amnt == 1

    validateBurn :: (CurrencySymbol, TokenName, Integer) -> Bool
    validateBurn (_, _, amnt) = amnt == (-1) && traceIfFalse "invalid timenft burn dead line" checkDeadline

    validate :: Bool
    validate = validateMint (PlutusTx.Prelude.head mintedFlattened) ||
               validateBurn (PlutusTx.Prelude.head mintedFlattened)

timeNft :: Script
timeNft = fromCompiledCode $$(PlutusTx.compile [|| mkPolicy ||])
