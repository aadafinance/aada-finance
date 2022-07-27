module Common.Utils where

import Ledger

{-# INLINABLE info #-}
info :: ScriptContext -> TxInfo
info = scriptContextTxInfo
