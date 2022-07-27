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
import qualified Common.Utils             as U

{-# INLINABLE mkPolicy #-}
mkPolicy :: POSIXTime -> ScriptContext -> Bool
mkPolicy mintingdate ctx = validate
  where
    tokenNameIsCorrect :: TokenName -> Bool
    tokenNameIsCorrect tn = fromBuiltin $ equalsByteString (unTokenName tn) (U.intToByteString $ getPOSIXTime mintingdate)

    checkDeadline :: Bool
    checkDeadline = contains (from mintingdate) (U.range ctx)

    timeNftFilter :: (CurrencySymbol, TokenName, Integer) -> Bool
    timeNftFilter (cs, tn, _) = cs == ownCurrencySymbol ctx && tokenNameIsCorrect tn

    mintedFlattened :: [(CurrencySymbol, TokenName, Integer)]
    mintedFlattened = filter timeNftFilter $ flattenValue $ txInfoMint (U.info ctx)

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
