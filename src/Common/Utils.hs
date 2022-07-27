module Common.Utils where

import Ledger
import PlutusTx.Prelude
import qualified PlutusTx.Builtins.Internal as B
import Plutus.V1.Ledger.Value

{-# INLINABLE info #-}
info :: ScriptContext -> TxInfo
info = scriptContextTxInfo

{-# INLINABLE hasUTxO #-}
hasUTxO :: TxOutRef -> ScriptContext -> Bool
hasUTxO utxo ctx = traceIfFalse "No minting policy specified utxo found" $ any (\i -> txInInfoOutRef i == utxo) $ txInfoInputs (info ctx)

{-# INLINEABLE intToByteString #-}
intToByteString :: Integer -> BuiltinByteString
intToByteString x = if x `B.divideInteger` 10 == 0 then digitToByteString x
  else
    B.appendByteString (intToByteString (x `B.divideInteger` 10)) (digitToByteString (x `B.modInteger` 10))
       where
         digitToByteString :: Integer -> BuiltinByteString
         digitToByteString d = B.consByteString (d `B.addInteger` asciZero) B.emptyByteString

         asciZero :: Integer
         asciZero = 48

{-# INLINEABLE range #-}
range :: ScriptContext -> POSIXTimeRange
range ctx = txInfoValidRange (info ctx)

{-# INLINEABLE mintFlattened #-}
mintFlattened :: ScriptContext -> [(CurrencySymbol, TokenName, Integer)]
mintFlattened ctx = flattenValue $ txInfoMint (info ctx)

{-# INLINEABLE ownInput #-}
ownInput :: ScriptContext -> Maybe TxOut
ownInput ctx = do
    i <- findOwnInput ctx
    Just $ txInInfoResolved i

{-# INLINEABLE ownValue #-}
ownValue :: ScriptContext -> Maybe Value
ownValue ctx = do
    txo <- ownInput ctx
    pure $ txOutValue txo

{-# INLINEABLE valueToSc #-}
valueToSc :: ValidatorHash -> ScriptContext -> Value
valueToSc vh ctx = foldr (\(_, y) acc -> y <> acc) (PlutusTx.Prelude.mempty :: Value) (scriptOutputsAt vh (info ctx))