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
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}

module Liquidator.StRedeemer.StRedeemer where

import qualified PlutusTx
import Plutus.V1.Ledger.Api
import           Prelude              (Show (..))

data STRedeemer = STRedeemer {
    utxo                    :: TxOutRef
  , liquidateInterestScAddr :: Address
  , safetyModule            :: Address
} deriving (Show)

PlutusTx.makeIsDataIndexed ''STRedeemer [('STRedeemer, 0)]