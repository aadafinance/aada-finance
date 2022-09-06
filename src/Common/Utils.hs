module Common.Utils where

import Ledger
import PlutusTx.Prelude
import Plutus.V1.Ledger.Value

{-# INLINABLE info #-}
info :: ScriptContext -> TxInfo
info = scriptContextTxInfo

{-# INLINABLE hasUTxO #-}
hasUTxO :: TxOutRef -> ScriptContext -> Bool
hasUTxO utxo ctx = any (\i -> txInInfoOutRef i == utxo) $ txInfoInputs (info ctx)

{-# INLINEABLE range #-}
range :: ScriptContext -> POSIXTimeRange
range ctx = txInfoValidRange (info ctx)

{-# INLINEABLE mintFlattened #-}
mintFlattened :: ScriptContext -> [(CurrencySymbol, TokenName, Integer)]
mintFlattened ctx = flattenValue $ txInfoMint (info ctx)

{-# INLINEABLE valuePaidToAddress #-}
valuePaidToAddress :: ScriptContext -> Address -> Value
valuePaidToAddress ctx addr = mconcat
  (fmap txOutValue (filter (\x -> txOutAddress x == addr)
  (txInfoOutputs (info ctx))))

{-# INLINEABLE getUpperBound #-}
getUpperBound :: ScriptContext -> Maybe POSIXTime
getUpperBound ctx = case ivTo (range ctx) of
    UpperBound (Finite x) _ -> Just x
    _                       -> Nothing
