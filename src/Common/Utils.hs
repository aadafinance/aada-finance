module Common.Utils where

import Ledger
import PlutusTx.Prelude
import Plutus.V1.Ledger.Value
import Plutus.V1.Ledger.Api
import PlutusTx.Builtins

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

{-# INLINEABLE getLowerBound #-}
getLowerBound :: ScriptContext -> Maybe POSIXTime
getLowerBound ctx = case ivFrom (range ctx) of
    LowerBound (Finite x) _ -> Just x
    _                       -> Nothing

{-# INLINEABLE calculateTokenNameHash #-}
calculateTokenNameHash :: TxOutRef -> BuiltinByteString
calculateTokenNameHash utxo =
  sha2_256 (consByteString (txOutRefIdx utxo) ((getTxId . txOutRefId) utxo))

{-# INLINEABLE validateTokenName #-}
validateTokenName :: TokenName -> TxOutRef  -> Bool
validateTokenName tn utxo = unTokenName tn == calculateTokenNameHash utxo && txOutRefIdx utxo < 256

{-# INLINEABLE serialiseAddress #-}
serialiseAddress :: Address -> BuiltinByteString
serialiseAddress addr = case addr of
  Address (PubKeyCredential addrCred) Nothing -> getPubKeyHash addrCred
  Address (ScriptCredential (ValidatorHash addrCred)) Nothing -> addrCred
  Address (PubKeyCredential addrCred) (Just (StakingHash (PubKeyCredential stakingCred))) -> appendByteString (getPubKeyHash addrCred) (getPubKeyHash stakingCred)
  Address (PubKeyCredential addrCred) (Just (StakingHash (ScriptCredential (ValidatorHash stakingCred)))) -> appendByteString (getPubKeyHash addrCred) stakingCred
  Address (ScriptCredential (ValidatorHash addrCred)) (Just (StakingHash (ScriptCredential (ValidatorHash stakingCred)))) -> appendByteString addrCred stakingCred
  Address (ScriptCredential (ValidatorHash addrCred)) (Just (StakingHash (PubKeyCredential stakingCred))) -> appendByteString addrCred (getPubKeyHash stakingCred)
  Address (PubKeyCredential addrCred) (Just (StakingPtr one two three)) -> consByteString three (consByteString two (consByteString one (getPubKeyHash addrCred)))
  Address (ScriptCredential (ValidatorHash addrCred)) (Just (StakingPtr one two three)) -> consByteString three (consByteString two (consByteString one addrCred))

{-# INLINEABLE calculateTokenNameHash' #-}
calculateTokenNameHash' :: Address -> Address -> TxOutRef -> BuiltinByteString
calculateTokenNameHash' addr1 addr2 utxo =
  sha2_256 $ appendByteString (appendByteString addrpart1 addrpart2) utxopart
  where
    utxopart = consByteString (txOutRefIdx utxo) ((getTxId . txOutRefId) utxo)
    addrpart1 = serialiseAddress addr1
    addrpart2 = serialiseAddress addr2

{-# INLINEABLE validateTokenName' #-}
validateTokenName' :: TokenName -> Address -> Address -> TxOutRef  -> Bool
validateTokenName' tn addr1 addr2 utxo = unTokenName tn == calculateTokenNameHash' addr1 addr2 utxo && txOutRefIdx utxo < 256
