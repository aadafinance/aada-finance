module Common.Utils where

import Ledger
import PlutusTx.Prelude
import qualified PlutusTx.Builtins.Internal as B

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