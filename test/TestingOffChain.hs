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
import           OracleNft
import qualified Plutus.V1.Ledger.Ada as Ada

data TestingStatus = TestingStatus
    { boref :: !TxOutRef
    , bpkh  :: !PaymentPubKeyHash
    , loref :: !TxOutRef
    , lpkh  :: !PaymentPubKeyHash
    } deriving (Show, Generic, FromJSON, ToJSON, Prelude.Eq)

getTestDatum :: CurrencySymbol -> CurrencySymbol -> PaymentPubKeyHash -> RequestDatum
getTestDatum bNftCs liqNft pkh = RequestDatum
          { borrowersNFT      = bNftCs
          , borrowersPkh      = pkh
          , loantn            = "CONYMONY"
          , loancs            = "ffff"
          , loanamnt          = 150
          , interesttn        = "MONY"
          , interestcs        = "ff"
          , interestamnt      = 50
          , collateralcs      = "ffffff"
          , repayinterval     = 0
          , liquidateNft      = liqNft
        }

getTestRedeemer :: POSIXTime -> Collateral.CollateralRedeemer
getTestRedeemer t = Collateral.CollateralRedeemer
  { Collateral.mintdate        = POSIXTime getTimeNftDl
  , Collateral.interestPayDate = t
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

getFakeTxOutRef :: TxOutRef
getFakeTxOutRef = TxOutRef "ff" 0

lock :: AsContractError e => Contract (Last TestingStatus) s e ()
lock = do
    oref <- getUnspentOutput -- Contract w s e TxOutRef
    o    <- fromJust <$> Contract.txOutFromRef oref
    pkh  <- Contract.ownPaymentPubKeyHash

    let liqNftCs = scriptCurrencySymbol $ OracleNft.policy getFakeTxOutRef "ff" "ff" "ff" "ff" "ff"

    let borrowersNftPolicy = BorrowerNft.policy oref
    let lookups = Constraints.mintingPolicy borrowersNftPolicy <>
                  Constraints.unspentOutputs (Map.singleton oref o)

    let cs     = scriptCurrencySymbol borrowersNftPolicy
        val    = Value.singleton cs "B" (1 :: Integer) -- BorrowerNft.borrower (1 :: Integer)
        collat = Value.singleton "ffffff" "CONY" 100
        sc1val = Ada.lovelaceValueOf 2000000 <> collat
        bval   = Ada.lovelaceValueOf 2000000 <> val

    let datum = getTestDatum cs liqNftCs pkh
        sc1vh = validatorHash $ requestValidator getSc1Params
        dat = Datum $ PlutusTx.toBuiltinData datum
        constraints = TxConstraints.mustSpendPubKeyOutput oref <>
                      Constraints.mustPayToOtherScript sc1vh dat sc1val <>
                      TxConstraints.mustMintValueWithRedeemer (Redeemer (PlutusTx.toBuiltinData (0 :: Integer))) val <>
                      TxConstraints.mustPayToPubKey pkh bval

    ledgerTx <- submitTxConstraintsWith @Void lookups constraints
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    -- logInfo @String $ printf "locked lovelace with datum %s" (show datum)
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

    let liqNftCs = scriptCurrencySymbol $ OracleNft.policy getFakeTxOutRef "ff" "ff" "ff" "ff" "ff"

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

              datum = getTestDatum (scriptCurrencySymbol $ BorrowerNft.policy $ boref ts) liqNftCs (bpkh ts)
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

  now <- currentTime

  let rdm = Redeemer $ PlutusTx.toBuiltinData (getTestRedeemer now)
  let mintRdm = Redeemer $ Builtins.mkI 0

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
                    Constraints.mustMintValueWithRedeemer mintRdm bnftval <>
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
  now <- currentTime

  let timeRdm = Redeemer $ Builtins.mkI getTimeNftDl
      rdm     = Redeemer $ PlutusTx.toBuiltinData (getTestRedeemer now)
      mintRdm = Redeemer $ Builtins.mkI 0

  let sc2valh = validatorHash $ Collateral.validator getSc2Params

  let lendersMintingPolicy = LenderNft.policy sc2valh (loref ts)
      lenderNftVal        = Value.singleton (scriptCurrencySymbol lendersMintingPolicy) "L" (-2)
  let timePolicy = TimeNft.policy

  let minAda        = Ada.lovelaceValueOf 2000000
      timeNftVal    = Value.singleton (scriptCurrencySymbol timePolicy) getTimeNftDlTn (-1)
      collatVal     = Value.singleton "ffffff" "CONY" 100
      valToLender   = minAda <> collatVal

  let lookups = Constraints.mintingPolicy lendersMintingPolicy <>
                Constraints.mintingPolicy timePolicy  <>
                Constraints.otherScript (Collateral.validator getSc2Params) <>
                Constraints.unspentOutputs (uncurry Map.singleton soref)

  let constraints = TxConstraints.mustSpendScriptOutput (fst soref) rdm <>
                    Constraints.mustMintValueWithRedeemer mintRdm lenderNftVal <>
                    Constraints.mustMintValueWithRedeemer timeRdm timeNftVal <>
                    TxConstraints.mustPayToPubKey pkh valToLender <>
                    Constraints.mustValidateIn (from now)

  ledgerTx <- submitTxConstraintsWith @Void lookups constraints
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  logInfo @String $ "Lender canceled all good"

lenderCancelLoanLiquidate :: AsContractError e => TestingStatus -> Contract (Last TestingStatus) s e ()
lenderCancelLoanLiquidate ts = do
  let sc2Address = scriptHashAddress $ validatorHash $ Collateral.validator getSc2Params
  utxos <- utxosAt sc2Address
  let soref = head $ Map.toList utxos

  pkh  <- Contract.ownPaymentPubKeyHash  -- Lender pkh
  now <- currentTime

  let timeRdm = Redeemer $ Builtins.mkI getTimeNftDl
      rdm     = Redeemer $ PlutusTx.toBuiltinData (getTestRedeemer now)
      mintRdm = Redeemer $ Builtins.mkI 0
  let sc2valh = validatorHash $ Collateral.validator getSc2Params

  let lendersMintingPolicy = LenderNft.policy sc2valh (loref ts)
  let timePolicy = TimeNft.policy
  let liquidatePolicy = OracleNft.policy

  let minAda          = Ada.lovelaceValueOf 2000000
      timeNftVal      = Value.singleton (scriptCurrencySymbol timePolicy) getTimeNftDlTn 1
      collatVal       = Value.singleton "ffffff" "CONY" 100
      lenderNftVal    = Value.singleton (scriptCurrencySymbol lendersMintingPolicy) "L" 1
      liquidateNftVal = Value.singleton (scriptCurrencySymbol $ liquidatePolicy getFakeTxOutRef "ff" "ff" "ff" "ff" "ff") "ORACLE" 0
      valToLender     = minAda <> collatVal <> timeNftVal <> lenderNftVal <> liquidateNftVal

  logInfo @String $ printf "Value to lender: %s" (show valToLender)

  let lookups = Constraints.otherScript (Collateral.validator getSc2Params) <>
                Constraints.mintingPolicy (liquidatePolicy getFakeTxOutRef "ff" "ff" "ff" "ff" "ff") <>
                Constraints.unspentOutputs (uncurry Map.singleton soref)

  let constraints = TxConstraints.mustSpendScriptOutput (fst soref) rdm <>
                    TxConstraints.mustMintValueWithRedeemer mintRdm liquidateNftVal <>
                    TxConstraints.mustPayToPubKey pkh valToLender

  ledgerTx <- submitTxConstraintsWith @Void lookups constraints
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  logInfo @String $ "Lender liquidate all good"

returnLoan :: AsContractError e => TestingStatus -> Contract (Last TestingStatus) s e ()
returnLoan ts = do
  let sc2Address = scriptHashAddress $ validatorHash $ Collateral.validator getSc2Params
  utxos <- utxosAt sc2Address
  let soref = head $ Map.toList utxos

  now <- currentTime

  let timeRdm = Redeemer $ Builtins.mkI getTimeNftDl
      rdm     = Redeemer $ PlutusTx.toBuiltinData (getTestRedeemer now)
      mintRdm = Redeemer $ Builtins.mkI 0

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

  let constraints = TxConstraints.mustSpendScriptOutput (fst soref) rdm <>
                    Constraints.mustMintValueWithRedeemer mintRdm borrowersNftVal <>
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

-- createOracle :: AsContractError e => Contract w s e ()
-- createOracle = do
--     let oracleVh = scriptHashAddress $ validatorHash Oracle.oracleValidator
--     let minval   = Ada.lovelaceValueOf 2000000

--     -- lock cony
--     oref  <- getUnspentOutput
--     o     <- fromJust <$> Contract.txOutFromRef oref

--     let dat = Datum $ PlutusTx.toBuiltinData getConyOracleDatum

--     let lookups     = Constraints.unspentOutputs (Map.singleton oref o)
--     let constraints = Constraints.mustPayToOtherScript (validatorHash Oracle.oracleValidator) dat minval

--     ledgerTx <- submitTxConstraintsWith @Void lookups constraints
--     void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
--     logInfo @String $ printf "first oracle utxo locked. Cony: %s" (show getConyOracleDatum)

--     -- lock mony
--     oref  <- getUnspentOutput
--     o     <- fromJust <$> Contract.txOutFromRef oref

--     let dat = Datum $ PlutusTx.toBuiltinData getMonyOracleDatum

--     let lookups     = Constraints.unspentOutputs (Map.singleton oref o)
--     let constraints = Constraints.mustPayToOtherScript (validatorHash Oracle.oracleValidator) dat minval

--     ledgerTx <- submitTxConstraintsWith @Void lookups constraints
--     void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
--     logInfo @String $ printf "second oracle utxo locked. Mony: %s" (show getMonyOracleDatum)

--     -- lock conymony
--     oref  <- getUnspentOutput
--     o     <- fromJust <$> Contract.txOutFromRef oref

--     let dat = Datum $ PlutusTx.toBuiltinData getConyMonyOracleDatum

--     let lookups     = Constraints.unspentOutputs (Map.singleton oref o)
--     let constraints = Constraints.mustPayToOtherScript (validatorHash Oracle.oracleValidator) dat minval

--     ledgerTx <- submitTxConstraintsWith @Void lookups constraints
--     void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
--     logInfo @String $ printf "third oracle utxo locked. ConyMony: %s" (show getConyMonyOracleDatum)