{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import qualified Data.Map                   as Map
import           Control.Monad              hiding (fmap)
import           Plutus.Contract            as Contract hiding (waitNSlots)
import           Plutus.V1.Ledger.Ada
import           Plutus.Trace.Emulator
    (EmulatorTrace, observableState, activateContractWallet, waitNSlots, runEmulatorTraceIO', EmulatorConfig(..) )
import           PlutusTx.Prelude           hiding (Semigroup(..), unless)
import           Prelude                    (IO, (<>), String)
import           Data.Monoid                  (Last (..))
import           Data.Text (Text)

import           Wallet.Emulator.Wallet
import           Ledger.Value
import           Data.Default (def, Default (..))
import           Control.Monad.Freer.Extras                   as Extras
import           TestingOffChain

main :: IO ()
main = runEmulatorTraceIO' def emCfg happyPathTrace

collateralValue :: Value
collateralValue = singleton "ffffff" "CONY" 100

loanValue :: Value
loanValue = singleton "ffff" "CONYMONY" 150

interestValue :: Value
interestValue = singleton "ff" "MONY" 50

happyPathTrace :: EmulatorTrace ()
happyPathTrace = do
    let w1 = knownWallet 1
    let w2 = knownWallet 2
    cw1 <- activateContractWallet w1 (lock :: Contract (Last TestingStatus) Empty Text ())
    void $ waitNSlots 2
    Last m <- observableState cw1
    case m of
        Nothing -> Extras.logError @String "Testing status wasn't set"
        Just ts -> do
            cw2 <- activateContractWallet w2 ((lend :: TestingStatus -> Contract (Last TestingStatus) Empty Text ()) ts)
            void $ waitNSlots 2
            Last m2 <- observableState cw2
            case m2 of
                Nothing -> Extras.logError @String "Updated testing status wasn't set"
                Just ts2 -> do 
                    _ <- activateContractWallet w1 ((returnLoan :: TestingStatus -> Contract (Last TestingStatus) Empty Text ()) ts2)
                    void $ waitNSlots 2
                    _ <- activateContractWallet  w2 ((retrieve :: TestingStatus -> Contract (Last TestingStatus) Empty Text ()) ts2)
                    void $ waitNSlots 2

borrowerCancelsLoan :: EmulatorTrace ()
borrowerCancelsLoan = do
    let w1 = knownWallet 1
    cw1 <- activateContractWallet w1 (lock :: Contract (Last TestingStatus) Empty Text ())
    void $ waitNSlots 2
    Last m <- observableState cw1
    case m of
        Nothing -> Extras.logError @String "Testing status wasn't set"
        Just ts -> do
            _ <- activateContractWallet w1 ((borrowerCancelLoan :: TestingStatus -> Contract (Last TestingStatus) Empty Text ()) ts)
            void $ waitNSlots 2

lenderCancelsLoan :: EmulatorTrace ()
lenderCancelsLoan = do
    let w1 = knownWallet 1
    let w2 = knownWallet 2
    cw1 <- activateContractWallet w1 (lock :: Contract (Last TestingStatus) Empty Text ())
    void $ waitNSlots 2
    Last m <- observableState cw1
    case m of
        Nothing -> Extras.logError @String "Testing status wasn't set"
        Just ts -> do
            cw2 <- activateContractWallet w2 ((lend :: TestingStatus -> Contract (Last TestingStatus) Empty Text ()) ts)
            void $ waitNSlots 2
            Last m2 <- observableState cw2
            case m2 of
                Nothing -> Extras.logError @String "Updated testing status wasn't set"
                Just ts2 -> do 
                    _ <- activateContractWallet w2 ((lenderCancelLoan :: TestingStatus -> Contract (Last TestingStatus) Empty Text ()) ts2)
                    void $ waitNSlots 2

emCfg :: EmulatorConfig
emCfg = EmulatorConfig (Left $ Map.fromList xs) def def
    where
        xs = [(knownWallet 1, ada1), (knownWallet 2, ada2)]
        ada1 = lovelaceValueOf 100_000_000 <> collateralValue <> interestValue
        ada2 = lovelaceValueOf 100_000_000 <> loanValue