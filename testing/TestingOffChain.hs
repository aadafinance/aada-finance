{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module TestingOffChain where

import           Control.Monad          hiding (fmap)
import           Data.Aeson                  (FromJSON, ToJSON)
import qualified Data.Map                    as Map
import           Data.Maybe (fromJust)
import           Data.Void (Void)
import           GHC.Generics                (Generic)
import           Ledger                 hiding (mint, singleton)
import           Ledger.Constraints          as Constraints
import qualified Ledger.Constraints.TxConstraints as TxConstraints
import           Ledger.Value                as Value
import           Plutus.Contract             as Contract
import           Plutus.Contract.Wallet
import           PlutusTx.Builtins           as Builtins
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import qualified PlutusTx
import           Data.Monoid                  (Last (..))
import           Prelude                     (Semigroup (..), Show (..), String, Eq)
import           Text.Printf            (printf)

import           Request
import qualified Collateral
import qualified Interest
import qualified TimeNft
import qualified BorrowerNft
import qualified LenderNft
import qualified Plutus.V1.Ledger.Ada as Ada

data TestingStatus = TestingStatus
    { boref :: !TxOutRef
    , bpkh  :: !PaymentPubKeyHash
    , loref :: !TxOutRef
    , lpkh  :: !PaymentPubKeyHash
    } deriving (Show, Generic, FromJSON, ToJSON, Prelude.Eq)

getTestDatum :: CurrencySymbol -> PaymentPubKeyHash -> RequestDatum
getTestDatum bNftCs pkh = RequestDatum {
            borrowersNFT      = bNftCs
          , borrowersPkh      = pkh
          , loantn            = "CONYMONY"
          , loancs            = "ffff"
          , loanamnt          = 150
          , interesttn        = "MONY"
          , interestcs        = "ff"
          , interestamnt      = 50
          , collateralcs      = "ffffff"
          , repayinterval     = 0
        }

getTimeNftDl :: Integer
getTimeNftDl = 0

getTimeNftDlTn :: TokenName
getTimeNftDlTn = "0"

getSc1Params :: Request.ContractInfo
getSc1Params = Request.ContractInfo {
        Request.borrower        = "B"
      , Request.lender          = "L"
      , Request.collateralcsvh  = validatorHash $ Collateral.validator getSc2Params
      , Request.timeNft         = scriptCurrencySymbol TimeNft.policy
    }

getSc2Params :: Collateral.ContractInfo
getSc2Params = Collateral.ContractInfo {
        Collateral.borrower     = "B"
      , Collateral.lender       = "L"
      , Collateral.interestscvh = validatorHash Interest.validator
      , Collateral.timeNft      = scriptCurrencySymbol TimeNft.policy
    }

lock :: AsContractError e => Contract (Last TestingStatus) s e ()
lock = do
    oref <- getUnspentOutput -- Contract w s e TxOutRef
    o    <- fromJust <$> Contract.txOutFromRef oref
    pkh  <- Contract.ownPaymentPubKeyHash

    let borrowersNftPolicy = BorrowerNft.policy oref
    let lookups = Constraints.mintingPolicy borrowersNftPolicy <>
                  Constraints.unspentOutputs (Map.singleton oref o)

    let cs     = scriptCurrencySymbol borrowersNftPolicy
        val    = Value.singleton cs "B" (1 :: Integer) -- BorrowerNft.borrower (1 :: Integer)
        collat = Value.singleton "ffffff" "CONY" 100
        sc1val = Ada.lovelaceValueOf 2000000 <> collat
        bval   = Ada.lovelaceValueOf 2000000 <> val

    let datum = getTestDatum cs pkh
        sc1vh = validatorHash $ requestValidator getSc1Params
        dat = Datum $ PlutusTx.toBuiltinData datum
        constraints = TxConstraints.mustSpendPubKeyOutput oref <>
                      Constraints.mustPayToOtherScript sc1vh dat sc1val <>
                      TxConstraints.mustMintValueWithRedeemer (Redeemer (PlutusTx.toBuiltinData (0 :: Integer))) val <>
                      TxConstraints.mustPayToPubKey pkh bval

    ledgerTx <- submitTxConstraintsWith @Void lookups constraints
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String $ printf "locked lovelace with datum %s" (show datum)
    let lr = TestingStatus oref pkh oref pkh
    tell $ Last $ Just lr
    logInfo @String "Lock finihesd!!!"

lend :: AsContractError e => TestingStatus -> Contract (Last TestingStatus) s e ()
lend ts = do
    logInfo @String "Lend"
    let rdm = Redeemer $ Builtins.mkI 0
    let sc1Address = scriptHashAddress $ validatorHash $ requestValidator getSc1Params

    utxos <- utxosAt sc1Address
    oref <- getUnspentOutput
    pkh  <- Contract.ownPaymentPubKeyHash

    let soref   = head $ Map.toList utxos

    let sc2valh = validatorHash $ Collateral.validator getSc2Params
    let lendersMintingPolicy = LenderNft.policy sc2valh oref
    let timePolicy = TimeNft.policy
    let dl = getTimeNftDl
    let timeRdm = Redeemer $ Builtins.mkI dl

    if Map.null utxos
        then logInfo @String $ "No utxo in script found"
        else do
          let lookups = Constraints.mintingPolicy lendersMintingPolicy <>
                        Constraints.mintingPolicy timePolicy <>
                        Constraints.otherScript (requestValidator getSc1Params) <>
                        Constraints.unspentOutputs (uncurry Map.singleton soref)

              minAda = Ada.lovelaceValueOf 2000000
              timeNftVal = Value.singleton (scriptCurrencySymbol timePolicy) getTimeNftDlTn 1
              lenderNftVal = Value.singleton (scriptCurrencySymbol lendersMintingPolicy) "L" 2
              singleLNftVal = Value.singleton (scriptCurrencySymbol lendersMintingPolicy) "L" 1
              loanVal = Value.singleton "ffff" "CONYMONY" 150
              sc1val = Value.singleton "ffffff" "CONY" 100 <>
                       minAda <>
                       singleLNftVal <>
                       timeNftVal

              datum = getTestDatum (scriptCurrencySymbol $ BorrowerNft.policy $ boref ts) (bpkh ts)
              dat   = Datum $ PlutusTx.toBuiltinData datum

          let  tx     = TxConstraints.mustSpendScriptOutput (fst soref) rdm <>
                        Constraints.mustMintValueWithRedeemer timeRdm timeNftVal <>
                        Constraints.mustMintValueWithRedeemer rdm lenderNftVal <>
                        TxConstraints.mustPayToPubKey (bpkh ts) (loanVal <> minAda) <>
                        TxConstraints.mustPayToOtherScript sc2valh dat sc1val <>
                        TxConstraints.mustPayToPubKey pkh (singleLNftVal <> minAda)
          ledgerTx <- submitTxConstraintsWith @Void lookups tx
          void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
          
    let lr = TestingStatus (boref ts) (bpkh ts) oref pkh
    tell $ Last $ Just lr
    logInfo @String $ "Lend all good"

borrowerCancelLoan :: AsContractError e => TestingStatus -> Contract (Last TestingStatus) s e ()
borrowerCancelLoan ts = do
  let sc1Address = scriptHashAddress $ validatorHash $ requestValidator getSc1Params
  utxos <- utxosAt sc1Address
  let soref = head $ Map.toList utxos

  oref <- getUnspentOutput
  o    <- fromJust <$> Contract.txOutFromRef oref
  pkh  <- Contract.ownPaymentPubKeyHash

  let rdm = Redeemer $ Builtins.mkI 0

  let borrowersNftPolicy = BorrowerNft.policy (boref ts)

  let cs      = scriptCurrencySymbol borrowersNftPolicy
      minAda  = Ada.lovelaceValueOf 2000000
      bnftval = Value.singleton cs "B" ((-1) :: Integer)
      collat  = Value.singleton "ffffff" "CONY" 100
      valtob  = collat <>  minAda

  let lookups = Constraints.mintingPolicy borrowersNftPolicy <>
                Constraints.unspentOutputs (Map.singleton oref o) <>
                Constraints.unspentOutputs (uncurry Map.singleton soref) <>
                Constraints.otherScript (requestValidator getSc1Params)

  let constraints = TxConstraints.mustSpendScriptOutput (fst soref) rdm <>
                    Constraints.mustMintValueWithRedeemer rdm bnftval <>
                    TxConstraints.mustPayToPubKey pkh valtob

  ledgerTx <- submitTxConstraintsWith @Void lookups constraints
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  logInfo @String $ "Borrower loan canceled?"

lenderCancelLoan :: AsContractError e => TestingStatus -> Contract (Last TestingStatus) s e ()
lenderCancelLoan ts = do
  let sc2Address = scriptHashAddress $ validatorHash $ Collateral.validator getSc2Params
  utxos <- utxosAt sc2Address
  let soref = head $ Map.toList utxos

  pkh  <- Contract.ownPaymentPubKeyHash  -- Lender pkh

  let timeRdm = Redeemer $ Builtins.mkI getTimeNftDl
      rdm     = Redeemer $ Builtins.mkI 0

  let sc2valh = validatorHash $ Collateral.validator getSc2Params

  let lendersMintingPolicy = LenderNft.policy sc2valh (loref ts)
      lenderNftVal        = Value.singleton (scriptCurrencySymbol lendersMintingPolicy) "L" (-2)
  let timePolicy = TimeNft.policy

  let minAda        = Ada.lovelaceValueOf 2000000
      timeNftVal    = Value.singleton (scriptCurrencySymbol timePolicy) getTimeNftDlTn (-1)
      collatVal     = Value.singleton "ffffff" "CONY" 100
      valToLender   = minAda <> collatVal

  now <- currentTime

  let lookups = Constraints.mintingPolicy lendersMintingPolicy <>
                Constraints.mintingPolicy timePolicy  <>
                Constraints.otherScript (Collateral.validator getSc2Params) <>
                Constraints.unspentOutputs (uncurry Map.singleton soref)

  let constraints = TxConstraints.mustSpendScriptOutput (fst soref) timeRdm <>
                    Constraints.mustMintValueWithRedeemer rdm lenderNftVal <>
                    Constraints.mustMintValueWithRedeemer timeRdm timeNftVal <>
                    TxConstraints.mustPayToPubKey pkh valToLender <>
                    Constraints.mustValidateIn (from now)

  ledgerTx <- submitTxConstraintsWith @Void lookups constraints
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  logInfo @String $ "Lender canceled all good"

returnLoan :: AsContractError e => TestingStatus -> Contract (Last TestingStatus) s e ()
returnLoan ts = do
  let sc2Address = scriptHashAddress $ validatorHash $ Collateral.validator getSc2Params
  utxos <- utxosAt sc2Address
  let soref = head $ Map.toList utxos

  let timeRdm = Redeemer $ Builtins.mkI getTimeNftDl
      rdm     = Redeemer $ Builtins.mkI 0

  let dat   = Datum $ Builtins.mkI 0

  let sc2valh = validatorHash $ Collateral.validator getSc2Params
  let lendersMintingPolicy = LenderNft.policy sc2valh (loref ts)

  let borrowersPolicy = BorrowerNft.policy $ boref ts
      borrowersCs     = scriptCurrencySymbol borrowersPolicy
      borrowersNftVal = Value.singleton borrowersCs "B" ((-1) :: Integer)
  let timePolicy = TimeNft.policy

  let sc3ValH = validatorHash Interest.validator

  let minAda        = Ada.lovelaceValueOf 2000000
      timeNftVal    = Value.singleton (scriptCurrencySymbol timePolicy) getTimeNftDlTn (-1)
      singleLNftVal = Value.singleton (scriptCurrencySymbol lendersMintingPolicy) "L" 1
      loanVal       = Value.singleton "ffff" "CONYMONY" 150
      interestVal   = Value.singleton "ff"   "MONY" 50
      collatVal     = Value.singleton "ffffff" "CONY" 100
      valToSc3      = minAda <> loanVal <> interestVal <> singleLNftVal

  now <- currentTime

  let lookups = Constraints.mintingPolicy borrowersPolicy <>
                Constraints.mintingPolicy timePolicy  <>
                Constraints.otherScript (Collateral.validator getSc2Params) <>
                Constraints.unspentOutputs (uncurry Map.singleton soref)

  let constraints = TxConstraints.mustSpendScriptOutput (fst soref) timeRdm <>
                    Constraints.mustMintValueWithRedeemer rdm borrowersNftVal <>
                    Constraints.mustMintValueWithRedeemer timeRdm timeNftVal <>
                    TxConstraints.mustPayToOtherScript sc3ValH dat valToSc3 <>
                    TxConstraints.mustPayToPubKey (bpkh ts) (minAda <> collatVal) <>
                    Constraints.mustValidateIn (from now)

  ledgerTx <- submitTxConstraintsWith @Void lookups constraints
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  logInfo @String $ "Return loan all good??"

retrieve :: AsContractError e => TestingStatus -> Contract (Last TestingStatus) s e ()
retrieve ts = do
  let sc3address = scriptHashAddress $ validatorHash Interest.validator
  utxos <- utxosAt sc3address
  let soref = head $ Map.toList utxos
  let rdm = Redeemer $ Builtins.mkI 0
  pkh  <- Contract.ownPaymentPubKeyHash
  let sc2valh = validatorHash $ Collateral.validator getSc2Params
  let lendersMintingPolicy = LenderNft.policy sc2valh (loref ts)
  let minAda        = Ada.lovelaceValueOf 2000000
      lenderNftVal  = Value.singleton (scriptCurrencySymbol lendersMintingPolicy) "L" (-2)
      loanVal       = Value.singleton "ffff" "CONYMONY" 150
      interestVal   = Value.singleton "ff"   "MONY" 50
      valToLender   = minAda <> loanVal <> interestVal

  let lookups = Constraints.mintingPolicy lendersMintingPolicy <>
                Constraints.otherScript Interest.validator <>
                Constraints.unspentOutputs (uncurry Map.singleton soref)

  let constraints = TxConstraints.mustSpendScriptOutput (fst soref) rdm <>
                    Constraints.mustMintValueWithRedeemer rdm lenderNftVal <>
                    TxConstraints.mustPayToPubKey pkh valToLender

  ledgerTx <- submitTxConstraintsWith @Void lookups constraints
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  logInfo @String $ "Retrieve All good"