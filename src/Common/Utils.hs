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
valueToSc vh ctx = mconcat $ fmap snd (scriptOutputsAt vh (info ctx))

{-# INLINEABLE csFromAsset #-}
csFromAsset :: AssetClass -> CurrencySymbol
csFromAsset as = fst $ unAssetClass as

{-# INLINEABLE tnFromAsset #-}
tnFromAsset :: AssetClass -> TokenName
tnFromAsset as = snd $ unAssetClass as
