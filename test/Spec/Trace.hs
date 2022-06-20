{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module Spec.Trace
    ( tests
    ) where

import           Control.Exception                            (try)
import           Control.Lens
import           Control.Monad                                hiding (fmap)
import           Control.Monad.Freer.Extras                   as Extras
import           Data.Default                                 (Default (..))
import           Data.IORef
import qualified Data.Map                                     as Map
import           Data.Monoid                                  (Last (..))
import           Ledger.Value                
import           Ledger.Ada                                   as Ada
import           Plutus.Contract.Test
import           Plutus.Contract.Test.Coverage
import           Plutus.Trace.Emulator                        as Emulator
import qualified PlutusTx.Prelude                             as Plutus
import           System.Exit                                  (ExitCode (..))
import           Test.Tasty
import qualified Test.Tasty.HUnit                             as HUnit
import           Plutus.Contract            as Contract hiding (waitNSlots)
import           TestingOffChain
import           Data.Text (Text)

tests :: TestTree
tests = checkPredicateOptions
    myOptions
    "token sale trace"
    myPredicate
    happyPathTrace

myOptions :: CheckOptions
myOptions = defaultCheckOptions & emulatorConfig .~ emCfg

myPredicate :: TracePredicate
myPredicate = walletFundsChange w1 negativeInterestValue .&&.
              walletFundsChange w2 interestValue

collateralValue :: Value
collateralValue = singleton "ffffff" "CONY" 100

loanValue :: Value
loanValue = singleton "ffff" "CONYMONY" 150

interestValue :: Value
interestValue = singleton "ff" "MONY" 50

negativeInterestValue :: Value
negativeInterestValue = singleton "ff" "MONY" (-50)

emCfg :: EmulatorConfig
emCfg = EmulatorConfig (Left $ Map.fromList xs) def def
    where
        xs = [(knownWallet 1, ada1), (knownWallet 2, ada2), (knownWallet 3, ada3)]
        ada1 = lovelaceValueOf 100_000_000 <> collateralValue <> interestValue
        ada2 = lovelaceValueOf 100_000_000 <> loanValue
        ada3 = lovelaceValueOf 100_000_000

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