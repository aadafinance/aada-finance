{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module TimeNft
  ( timeNft
  , timeNftShortBs
  , policy
  , intToByteString
  ) where

import           Cardano.Api.Shelley      (PlutusScript (..), PlutusScriptV1)
import           Codec.Serialise
import qualified Data.ByteString.Lazy     as LB
import qualified Data.ByteString.Short    as SBS
import           Ledger                   hiding (singleton)
import qualified Ledger.Typed.Scripts     as Scripts
import           Ledger.Value             as Value
import qualified PlutusTx
import           PlutusTx.Prelude         hiding (Semigroup (..), unless)
import           PlutusTx.Builtins.Internal as B

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
mkPolicy :: POSIXTime -> ScriptContext -> Bool
mkPolicy mintingdate ctx = validate
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

policy :: Scripts.MintingPolicy
policy = mkMintingPolicyScript $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy mkPolicy ||])

plutusScript :: Script
plutusScript = unMintingPolicyScript policy

validator :: Validator
validator = Validator plutusScript

scriptAsCbor :: LB.ByteString
scriptAsCbor = serialise validator

timeNft :: PlutusScript PlutusScriptV1
timeNft = PlutusScriptSerialised . SBS.toShort . LB.toStrict $ scriptAsCbor

timeNftShortBs :: SBS.ShortByteString
timeNftShortBs = SBS.toShort . LB.toStrict $ scriptAsCbor
