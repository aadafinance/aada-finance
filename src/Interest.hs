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
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}

module Interest
  ( interest
  ) where

import           Codec.Serialise
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS

import           Plutus.V2.Ledger.Contexts
import qualified Plutus.V1.Ledger.Scripts as Plutus
import           Plutus.V1.Ledger.Scripts
import           Plutus.V1.Ledger.Value
import qualified PlutusTx
import           PlutusTx (unsafeFromBuiltinData)
import           PlutusTx.Prelude hiding (Semigroup (..), unless)


{-# INLINABLE flattenBuiltinByteString #-}
flattenBuiltinByteString :: [BuiltinByteString] -> BuiltinByteString
flattenBuiltinByteString [] = emptyByteString
flattenBuiltinByteString (x:xs) = appendByteString x $ flattenBuiltinByteString xs

{-# INLINABLE lender #-}
lender :: TokenName
lender = TokenName { unTokenName = flattenBuiltinByteString [consByteString x emptyByteString | x <- [76]]}  -- L

{-# INLINABLE mkValidator #-}
mkValidator :: Integer -> BuiltinData -> BuiltinData -> ()
mkValidator _ _ (unsafeFromBuiltinData -> ctx :: ScriptContext) = check validate
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    mintFlattened :: [(CurrencySymbol, TokenName, Integer)]
    mintFlattened = flattenValue $ txInfoMint info

    ownInput :: Maybe TxOut
    ownInput = case findOwnInput ctx of
      Just txin -> Just $ txInInfoResolved txin
      Nothing   -> Nothing

    hasBurntNft :: CurrencySymbol -> Bool
    hasBurntNft cs = case ownInput of
      Just txo -> valueOf (txOutValue txo) cs lender == 1
      Nothing  -> False

    validate :: Bool
    validate = case mintFlattened of
      [(cs, tn, amt)] -> (amt == (-2)) &&
                         hasBurntNft cs &&
                         (tn == lender)
      _               -> False
interest :: Script
interest = fromCompiledCode $ $$(PlutusTx.compile [|| mkValidator ||])
