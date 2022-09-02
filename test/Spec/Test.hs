{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Spec.Test where

import Data.Either
import Prelude

import Test.Tasty

import Plutus.V1.Ledger.Api

import           Request
import qualified Collateral
import qualified Interest
import qualified AadaNft
import Helpers.TestMintingPolicy as OracleNft
import Plutus.Test.Model
import Ledger.Address (PaymentPubKeyHash(..))
import Ledger (validatorHash, scriptCurrencySymbol, interval)
import           Ledger.Value                as Value
import           PlutusTx
import qualified PlutusTx.Builtins.Internal as INT
import Collateral (CollateralDatum (loanDuration))
import Control.Monad.State.Strict
import Helpers.TestValidator
import qualified Data.ByteString.UTF8 as BSC
import Plutus.V1.Ledger.Ada (adaValueOf)

mainTests :: BchConfig -> TestTree
mainTests cfg =
  testGroup
    "Main tests"
    [
      testNoErrors (adaValue 10_000_000 <> borrowerInitialFunds <> lenderInitialFunds) cfg "Happy path" happyPath
    , testNoErrors (adaValue 10_000_000 <> borrowerInitialFunds <> lenderInitialFunds) cfg "Borrower cancels loan test" borrowerCancelsLoan
    , testNoErrors (adaValue 10_000_000 <> borrowerInitialFunds <> lenderInitialFunds) cfg "Borrower returns full interest when loan return time has passed" returnFullLoan
    , testNoErrors (adaValue 10_000_000 <> borrowerInitialFunds <> lenderInitialFunds) cfg "Borrower returns less than it should then full time has passed" (mustFail returnNotEnoughInterest)
    , testNoErrors (adaValue 10_000_000 <> borrowerInitialFunds <> lenderInitialFunds) cfg "Borrower returns loan when half the time passed returning less than full interest" returnPartialLoan
    , testNoErrors (adaValue 10_000_000 <> borrowerInitialFunds <> lenderInitialFunds) cfg "Borrower returns loan when half the time passed returning less than full interest with same currency" (mustFail returnPartialLoanSameCs)
    , testNoErrors (adaValue 10_000_000 <> borrowerInitialFunds <> lenderInitialFunds) cfg "Borrower returns less interest than it should because of forged mintDate" (mustFail returnPartialLoanForgedMintDate)
    , testNoErrors (adaValue 10_000_000 <> borrowerInitialFunds <> lenderInitialFunds) cfg "Borrower returns less interest than it should" (mustFail returnPartialLoanLessThanItShoudInterestRepayed)
    , testNoErrors (adaValue 10_000_000 <> borrowerInitialFunds <> lenderInitialFunds) cfg "test loan return expiration date. Loan request not-expired" provideLoanOnTime
    , testNoErrors (adaValue 10_000_000 <> borrowerInitialFunds <> lenderInitialFunds) cfg "test loan return expiration date. Loan request expired" (mustFail provideLoanNotOnTime)
    , testNoErrors (adaValue 10_000_000 <> borrowerInitialFunds <> lenderInitialFunds) cfg "liquidate borrower" liquidateBorrower
    , testNoErrors (adaValue 10_000_000 <> borrowerInitialFunds <> lenderInitialFunds'') cfg "Lender dos borrower" (mustFail lenderDosBorrower)
    , testNoErrors (adaValue 10_000_000 <> borrowerInitialFunds'' <> lenderInitialFunds) cfg "Borrower dos lender" (mustFail borrowerDosLender)
    ]

testSize :: BchConfig -> TestTree
testSize cfg =
  testGroup
    "tests to check transaction sizes"
    [
    --   testLimits (adaValue 10_000_000 <> borrowerInitialFunds <> lenderInitialFunds) cfg "Happy path"           id happyPath -- (happyPath >> logError "show stats")
    -- , testLimits (adaValue 10_000_000 <> borrowerInitialFunds <> lenderInitialFunds) cfg "Borrower liquidates"  id liquidateBorrower -- (liquidateBorrower >> logError "show stats")
    -- , testLimits (adaValue 10_000_000 <> borrowerInitialFunds <> lenderInitialFunds'') cfg "Lender dos borrower" id (lenderDosBorrower >> logError "show stats")
    -- , testLimits (adaValue 10_000_000 <> borrowerInitialFunds'' <> lenderInitialFunds) cfg "Borrower dos lender" id (borrowerDosLender >> logError "show stats")
    ]

type RepayInterval = POSIXTime
type RequestExpirationDate = POSIXTime
type LendDate = POSIXTime
type BorrowerTokenName = TokenName
type LenderTokenName = TokenName
type BorrowersAddressPkh = PubKeyHash
type LiquidationNftCs = CurrencySymbol

-- TODO move to utils section later
adaValue :: Integer -> Value
adaValue = singleton adaSymbol adaToken

setupUsers :: Run [PubKeyHash]
setupUsers = sequenceA [newUser borrowerInitialFunds, newUser lenderInitialFunds]

setupUsers' :: Run [PubKeyHash]
setupUsers' = sequenceA [newUser borrowerInitialFunds', newUser lenderInitialFunds']

setupUsers'' :: Run [PubKeyHash]
setupUsers'' = sequenceA [newUser borrowerInitialFunds, newUser lenderInitialFunds'']

setupUsers''' :: Run [PubKeyHash]
setupUsers''' = sequenceA [newUser borrowerInitialFunds'', newUser lenderInitialFunds]

setupSimpleNUsers :: Int -> Run [PubKeyHash]
setupSimpleNUsers n = replicateM n $ newUser $ adaValue 1000

-- TODO could this be done better?
fakeCoinCs :: FakeCoin -> CurrencySymbol
fakeCoinCs fc = case fakeCoin fc of
  AssetClass (cs, _) -> cs

collateralCoin :: FakeCoin
collateralCoin = FakeCoin "collateral-coin-CONY"

loanCoin :: FakeCoin
loanCoin = FakeCoin "loan-coin-CONYMONY"

interestCoin :: FakeCoin
interestCoin = FakeCoin "interest-coin-MONY"

generateFakeValues :: Int -> [Value]
generateFakeValues n = fmap (`fakeValue` 1) (FakeCoin . toBuiltin . BSC.fromChar <$> take n ['a'..])

generateFakeValues' :: Int -> Value
generateFakeValues' n = mconcat $ generateFakeValues n

borrowerInitialFunds :: Value
borrowerInitialFunds = fakeValue collateralCoin 100 <> fakeValue interestCoin 50 <> adaValue 100

lenderInitialFunds :: Value
lenderInitialFunds = fakeValue loanCoin 150 <> adaValue 100

borrowerInitialFunds' :: Value
borrowerInitialFunds' = fakeValue collateralCoin 100 <> fakeValue loanCoin 50 <> adaValue 100

lenderInitialFunds' :: Value
lenderInitialFunds' = fakeValue loanCoin 100 <> adaValue 100

lenderDosAmount :: Int
lenderDosAmount = 69 -- this is actually the limit when tx can go in, but then can't go out

borrowerDosAmount :: Int
borrowerDosAmount = 36

lenderInitialFunds'' :: Value
lenderInitialFunds'' = lenderInitialFunds <> generateFakeValues' lenderDosAmount

borrowerInitialFunds'' :: Value
borrowerInitialFunds'' = borrowerInitialFunds <> generateFakeValues' borrowerDosAmount

getLenderNftCs :: CurrencySymbol
getLenderNftCs = scriptCurrencySymbol getLenderNftPolicy

getBorrowerNftCs :: CurrencySymbol
getBorrowerNftCs = scriptCurrencySymbol getBorrowerNftPolicy

getLenderNftPolicy :: MintingPolicy
getLenderNftPolicy = AadaNft.policy True

getBorrowerNftPolicy :: MintingPolicy
getBorrowerNftPolicy = AadaNft.policy False

getSc1Params :: Request.ContractInfo
getSc1Params = Request.ContractInfo {
        Request.lenderNftCs    = getLenderNftCs
      , Request.borrowersNftCs = getBorrowerNftCs
      , Request.collateralSc   = Address (ScriptCredential (validatorHash $ Collateral.validator getSc2Params)) Nothing
    }

getSc2Params :: Collateral.ContractInfo
getSc2Params = Collateral.ContractInfo {
        Collateral.lenderNftCs    = getLenderNftCs
      , Collateral.borrowersNftCs = getBorrowerNftCs
      , Collateral.interestSc     = Address (ScriptCredential (validatorHash (Interest.validator (Interest.ContractInfo getLenderNftCs)))) Nothing
    }

getTestDatum :: RepayInterval -> BorrowerTokenName -> LiquidationNftCs -> BorrowersAddressPkh -> RequestExpirationDate -> LenderTokenName -> LendDate -> Maybe StakingCredential -> RequestDatum
getTestDatum returnt bNftTn liqNft pkh expiration ltn t staking = RequestDatum
  { borrowersNftTn        = bNftTn
  , borrowersAddress      = Address (PubKeyCredential pkh) staking -- (Just . StakingHash . PubKeyCredential . PubKeyHash $ "ff")
  , loan                  = assetClass (fakeCoinCs loanCoin) "loan-coin-CONYMONY"
  , loanAmnt              = 150
  , interest              = assetClass (fakeCoinCs interestCoin) "interest-coin-MONY"
  , interestAmnt          = 50
  , collateral            = assetClass (fakeCoinCs collateralCoin) "collateral-coin-CONY"
  , collateralAmnt        = 100                    -- amount of collateral
  , loanDuration         = returnt
  , liquidateNft          = liqNft
  , collateralFactor      = 5                      -- Colalteral factor used for liquidation
  , liquidationCommission = 150                    -- How much % borrower will pay for lender when liquidated (before time passes)
  , requestExpiration     = expiration
  , lenderNftTn           = ltn
  , lendDate              = t
  }

getTestDatum2 :: RepayInterval -> BorrowerTokenName -> LiquidationNftCs -> BorrowersAddressPkh -> RequestExpirationDate -> LenderTokenName -> LendDate -> Maybe StakingCredential -> RequestDatum
getTestDatum2 returnt bNftTn liqNft pkh expiration ltn t staking = RequestDatum
  { borrowersNftTn        = bNftTn
  , borrowersAddress      = Address (PubKeyCredential pkh) staking -- (Just . StakingHash . PubKeyCredential . PubKeyHash $ "ff")
  , loan                  = assetClass (fakeCoinCs loanCoin) "loan-coin-CONYMONY"
  , loanAmnt              = 100
  , interest              = assetClass (fakeCoinCs loanCoin) "loan-coin-CONYMONY"
  , interestAmnt          = 50
  , collateral            = assetClass (fakeCoinCs collateralCoin) "collateral-coin-CONY"
  , collateralAmnt        = 100                    -- amount of collateral
  , loanDuration         = returnt
  , liquidateNft          = liqNft
  , collateralFactor      = 5                      -- Colalteral factor used for liquidation
  , liquidationCommission = 150                    -- How much % borrower will pay for lender when liquidated (before time passes)
  , requestExpiration     = expiration
  , lenderNftTn           = ltn
  , lendDate              = t
  }

getCollatDatumFromRequestDat :: RequestDatum -> TokenName -> POSIXTime -> Collateral.CollateralDatum
getCollatDatumFromRequestDat rqDat@RequestDatum{..} newTn newMint = Collateral.CollateralDatum
          { Collateral.borrowersNftTn        = borrowersNftTn
          , Collateral.borrowersAddress      = borrowersAddress
          , Collateral.loan                  = loan
          , Collateral.loanAmnt              = loanAmnt
          , Collateral.interest              = interest
          , Collateral.interestAmnt          = interestAmnt
          , Collateral.collateral            = collateral
          , Collateral.collateralAmnt        = 100                    -- amount of collateral
          , Collateral.loanDuration         = loanDuration
          , Collateral.liquidateNft          = liquidateNft
          , Collateral.collateralFactor      = 5                      -- Colalteral factor used for liquidation
          , Collateral.liquidationCommission = 150
          , Collateral.requestExpiration     = requestExpiration
          , Collateral.lenderNftTn           = newTn
          , Collateral.lendDate              = newMint
        }

getAadaTokenName :: TxOutRef -> TokenName
getAadaTokenName utxo = TokenName $ INT.sha2_256 (INT.consByteString (txOutRefIdx utxo) ((getTxId . txOutRefId) utxo))

createLockFundsTx :: RepayInterval -> BorrowersAddressPkh -> TxOutRef -> UserSpend -> RequestExpirationDate -> LendDate -> LiquidationNftCs -> Tx
createLockFundsTx t pkh oref usp expiration mintDate oracle =
    mconcat
      [ userSpend usp
      , payToScript
        (requestTypedValidator getSc1Params)
        (getTestDatum t (getAadaTokenName oref) oracle pkh expiration "" mintDate Nothing)
        (fakeValue collateralCoin 100 <> adaValue 2)
      ]

getCancelRequestTx :: PubKeyHash -> Value -> RequestDatum -> TxOutRef -> TokenName -> Tx
getCancelRequestTx pkh val dat lockRef lenderTn =
    mconcat
      [ spendScript (requestTypedValidator getSc1Params) lockRef lenderTn dat
      , payToPubKey pkh val
      ]

getTxIn :: UserSpend -> RequestDatum -> TxOutRef -> TokenName -> Tx
getTxIn usp dat scriptTxOut lenderTn =
  mconcat
  [ spendScript (requestTypedValidator getSc1Params) scriptTxOut lenderTn dat
  , userSpend usp
  ]

getOracleNftTn :: TokenName
getOracleNftTn = TokenName "ff"

getLNftVal :: Integer -> CurrencySymbol -> TxOutRef -> Value
getLNftVal n cs utxo = Value.singleton cs (getAadaTokenName utxo) n

getBNftVal :: Integer -> CurrencySymbol -> TxOutRef -> Value
getBNftVal n cs utxo = Value.singleton cs (getAadaTokenName utxo) n

getMintBorrowerNftTx :: PubKeyHash -> TxOutRef -> Tx
getMintBorrowerNftTx pkh oref = addMintRedeemer getBorrowerNftPolicy oref $
  mconcat
    [ mintValue (AadaNft.policy False) (getBNftVal 1 cs oref)
    , payToPubKey pkh (adaValue 1 <> getBNftVal 1 cs oref)
    ]
  where
    cs  = scriptCurrencySymbol getBorrowerNftPolicy

-- getCancelRequestTx :: PubKeyHash -> Value -> RequestDatum -> TxOutRef -> Tx
-- getCancelRequestTx pkh val dat lockRef =
--     mconcat
--       [ spendScript (requestTypedValidator getSc1Params) lockRef 0 dat
--       , payToPubKey pkh val
--       ]

getTxOutLend :: PubKeyHash -> PubKeyHash -> Collateral.CollateralDatum -> TxOutRef -> Value -> Tx
getTxOutLend borrower lender dat utxo valToScript = addMintRedeemer getLenderNftPolicy utxo $
 mconcat
  [ mintValue getLenderNftPolicy (getLNftVal 1 getLenderNftCs utxo)
  , payToScript
      (Collateral.collateralTypedValidator getSc2Params)
      dat
      (fakeValue collateralCoin 100 <> adaValue 2 <> valToScript)
  , payToPubKey borrower (fakeValue loanCoin 150 <> adaValue 2)
  , payToPubKey lender (adaValue 2 <> getLNftVal 1 getLenderNftCs utxo)
  ]

getTxOutReturn :: Integer -> PubKeyHash ->  TokenName -> Value -> TxOutRef  -> Tx
getTxOutReturn interest borrower dat valToInt oref = addMintRedeemer getBorrowerNftPolicy oref $
 mconcat
  [ mintValue getBorrowerNftPolicy (getBNftVal (-1) getBorrowerNftCs oref)
  , payToScript
      (Interest.typedValidator (Interest.ContractInfo getLenderNftCs))
      dat
      (fakeValue loanCoin 150 <> fakeValue interestCoin interest <> adaValue 2 <> valToInt)
  , payToPubKey borrower (fakeValue collateralCoin 100 <> adaValue 3)
  ]

getTxInFromCollateral :: [UserSpend] -> Collateral.CollateralDatum -> Integer -> TxOutRef -> Tx
getTxInFromCollateral usps dat rdm scriptTxOut =
  mconcat
  (spendScript (Collateral.collateralTypedValidator getSc2Params) scriptTxOut rdm dat : fmap userSpend usps)

getBurnBorrowerNftTx ::  PubKeyHash -> TxOutRef -> UserSpend -> Tx
getBurnBorrowerNftTx pkh oref usp = addMintRedeemer getBorrowerNftPolicy oref $
  mconcat
    [ mintValue getBorrowerNftPolicy (getBNftVal (-1) getBorrowerNftCs oref)
    , payToPubKey pkh (adaValue 1)
    , userSpend usp
    ]

borrowerCancelsLoan :: Run Bool
borrowerCancelsLoan = do
  users <- setupUsers
  let u1       = head users
      valToPay = fakeValue collateralCoin 100 <> adaValue 3
  sp <- spend u1 valToPay
  let oref = getHeadRef sp
  let borrowerNftRef = oref
  let tx = createLockFundsTx 0 u1 oref sp 0 0 (scriptCurrencySymbol $ OracleNft.policy "ff" "ff" "ff" "ff" "ff") <> getMintBorrowerNftTx u1 oref
  submitTx u1 tx
  utxos <- utxoAt $ requestAddress getSc1Params
  let [(lockRef, _)] = utxos
  lockDat <- datumAt @RequestDatum lockRef
  case lockDat of
      Just dat -> do
          let valFromSc1 = fakeValue collateralCoin 100 <> adaValue 2
              valFromUsr = adaValue 1 <> getBNftVal 1 getBorrowerNftCs borrowerNftRef
          sp <- spend u1 valFromUsr
          tx <- signTx u1 $ getCancelRequestTx u1 valFromSc1 dat lockRef (getAadaTokenName lockRef) <> getBurnBorrowerNftTx u1 borrowerNftRef sp
          isRight <$> sendTx tx
      Nothing -> pure False

returnFullLoan :: Run Bool
returnFullLoan = do
  users <- setupUsers
  let borrower = head users
      lender   = last users
      valToPay = fakeValue collateralCoin 100 <> adaValue 2 <> adaValue 1
  sp <- spend borrower valToPay
  let oref = getHeadRef sp
  let borrowerNftRef = oref
  let tx = createLockFundsTx 0 borrower oref sp 100000 0 (scriptCurrencySymbol $ OracleNft.policy "ff" "ff" "ff" "ff" "ff") <> getMintBorrowerNftTx borrower oref

  submitTx borrower tx
  utxos <- utxoAt $ requestAddress getSc1Params -- utxoAt  :: HasAddress addr => addr -> Run [(TxOutRef, TxOut)]
  let [(lockRef, _)] = utxos
  let lenderNftRef = lockRef
  lockDat <- datumAt @RequestDatum lockRef
  case lockDat of
      Just dat -> do
          let mintTime = 7000
          let convertedDat        = getCollatDatumFromRequestDat dat (getAadaTokenName lenderNftRef) mintTime
              valForLenderToSpend = fakeValue loanCoin 150 <> adaValue 4
          sp <- spend lender valForLenderToSpend
          let tx = getTxIn sp dat lockRef (getAadaTokenName lenderNftRef) <> getTxOutLend borrower lender convertedDat lockRef (adaValueOf 0)
          logInfo $  "current time: " ++ show mintTime
          tx <- validateIn (interval 2000 6000) tx
          submitTx lender tx

          -- loan return phase

          let valTmp1 = getBNftVal 1 getBorrowerNftCs borrowerNftRef <>
                        adaValue 1
              valTmp2 = fakeValue loanCoin 150 <>
                        adaValue 1
              valTmp3 = fakeValue interestCoin 50 <>
                        adaValue 1
          wait 2000
          intPayDate <- currentTime

          sp1 <- spend borrower valTmp1
          sp2 <- spend borrower valTmp2
          sp3 <- spend borrower valTmp3

          utxos <- utxoAt $ Collateral.collateralAddress getSc2Params
          let [(lockRef, _)] = utxos

          let intDat = Collateral.lenderNftTn convertedDat

          let tx2 = getTxInFromCollateral [sp1, sp2, sp3] convertedDat 0 lockRef <>
                    getTxOutReturn 50 borrower intDat (adaValueOf 0) borrowerNftRef

          logInfo $  "int pay date time: " ++ show intPayDate
          tx2 <- validateIn (interval 6000 intPayDate) tx2
          submitTx lender tx2
          pure True
      Nothing -> pure False

returnNotEnoughInterest :: Run Bool
returnNotEnoughInterest = do
  users <- setupUsers
  let borrower = head users
      lender   = last users
      valToPay = fakeValue collateralCoin 100 <> adaValue 2 <> adaValue 1
  sp <- spend borrower valToPay
  let oref = getHeadRef sp
  let borrowerNftRef = oref
  let tx = createLockFundsTx 0 borrower oref sp 100000 0 (scriptCurrencySymbol $ OracleNft.policy "ff" "ff" "ff" "ff" "ff") <> getMintBorrowerNftTx borrower oref

  submitTx borrower tx
  utxos <- utxoAt $ requestAddress getSc1Params
  let [(lockRef, _)] = utxos
  let lenderNftRef = lockRef
  lockDat <- datumAt @RequestDatum lockRef
  case lockDat of
      Just dat -> do
          let mintTime = 7000
          let convertedDat        = getCollatDatumFromRequestDat dat (getAadaTokenName lenderNftRef) mintTime
              valForLenderToSpend = fakeValue loanCoin 150 <> adaValue 4
          sp <- spend lender valForLenderToSpend
          let tx = getTxIn sp dat lockRef (getAadaTokenName lenderNftRef) <> getTxOutLend borrower lender convertedDat lockRef (adaValueOf 0)

          logInfo $  "current time: " ++ show mintTime
          tx <- validateIn (interval 2000 6000) tx
          submitTx lender tx

          -- loan return phase

          let valTmp1 = getBNftVal 1 getBorrowerNftCs borrowerNftRef <>
                        adaValue 1
              valTmp2 = fakeValue loanCoin 150 <>
                        adaValue 1
              valTmp3 = fakeValue interestCoin 25 <>
                        adaValue 1
          intPayDate <- currentTime

          sp1 <- spend borrower valTmp1
          sp2 <- spend borrower valTmp2
          sp3 <- spend borrower valTmp3

          utxos <- utxoAt $ Collateral.collateralAddress getSc2Params
          let [(lockRef, _)] = utxos


          let intDat = Collateral.lenderNftTn convertedDat

          let tx2 = getTxInFromCollateral [sp1, sp2, sp3] convertedDat 0 lockRef <>
                    getTxOutReturn 25 borrower intDat (adaValueOf 0) borrowerNftRef
          tx2 <- validateIn (interval 6000 intPayDate) tx2
          submitTx lender tx2
          pure True
      Nothing -> pure False

returnPartialLoan :: Run Bool
returnPartialLoan = do
  users <- setupUsers
  let borrower = head users
      lender   = last users
      valToPay = fakeValue collateralCoin 100 <> adaValue 2 <> adaValue 1
  
  sp <- spend borrower valToPay
  
  let oref = getHeadRef sp
      borrowerNftRef = oref
      repayint = 20000
      tx = createLockFundsTx repayint borrower oref sp 100000 0 (scriptCurrencySymbol $ OracleNft.policy "ff" "ff" "ff" "ff" "ff") <> getMintBorrowerNftTx borrower oref
  
  submitTx borrower tx
  utxos <- utxoAt $ requestAddress getSc1Params
  let [(lockRef, _)] = utxos
  let lenderNftRef = lockRef
  lockDat <- datumAt @RequestDatum lockRef
  case lockDat of
      Just dat -> do
          let mintTime = 7000
          let convertedDat        = getCollatDatumFromRequestDat dat (getAadaTokenName lenderNftRef) mintTime
              valForLenderToSpend = fakeValue loanCoin 150 <> adaValue 4
          sp <- spend lender valForLenderToSpend
          let tx = getTxIn sp dat lockRef (getAadaTokenName lenderNftRef) <> getTxOutLend borrower lender convertedDat lockRef (adaValueOf 0)
          realCurTime <- currentTime
          logInfo $  "current time1: " ++ show realCurTime
          tx <- validateIn (interval 2000 6000) tx
          submitTx lender tx

          -- loan return phase

          let valTmp1 = getBNftVal 1 getBorrowerNftCs borrowerNftRef <>
                        adaValue 1
              valTmp2 = fakeValue loanCoin 150 <>
                        adaValue 1
              valTmp3 = fakeValue interestCoin 25 <>
                        adaValue 1
          wait 2000
          intPayDate <- currentTime
          logInfo $  "intPayDate: " ++ show intPayDate

          sp1 <- spend borrower valTmp1
          sp2 <- spend borrower valTmp2
          sp3 <- spend borrower valTmp3

          utxos <- utxoAt $ Collateral.collateralAddress getSc2Params
          let [(lockRef, _)] = utxos
          let intDat = Collateral.lenderNftTn convertedDat

          let tx2 = getTxInFromCollateral [sp1, sp2, sp3] convertedDat 0 lockRef <>
                                        getTxOutReturn 25 borrower intDat (adaValueOf 0) borrowerNftRef

          tx2 <- validateIn (interval 6000 (intPayDate + 2000)) tx2
          wait 2000
          time <- currentTime
          logInfo $  "time before repaying: " ++ show time
          submitTx lender tx2
          pure True
      Nothing -> pure False

createLockFundsTx2 :: POSIXTime -> PubKeyHash -> TxOutRef -> UserSpend -> POSIXTime -> POSIXTime -> Tx
createLockFundsTx2 t pkh oref usp expiration mintDate =
    mconcat
      [ userSpend usp
      , payToScript
        (requestTypedValidator getSc1Params)
        (getTestDatum2 t (getAadaTokenName oref) (scriptCurrencySymbol $ OracleNft.policy "ff" "ff" "ff" "ff" "ff") pkh expiration "" mintDate Nothing)
        (fakeValue collateralCoin 100 <> adaValue 2)
      ]

getTxOutLend2 :: PubKeyHash -> PubKeyHash -> Collateral.CollateralDatum -> TxOutRef -> Tx
getTxOutLend2 borrower lender dat utxo = addMintRedeemer getLenderNftPolicy utxo $
 mconcat
  [ mintValue getLenderNftPolicy (getLNftVal 1 getLenderNftCs utxo)
  , payToScript
      (Collateral.collateralTypedValidator getSc2Params)
      dat
      (fakeValue collateralCoin 100 <> adaValue 2)
  , payToPubKey borrower (fakeValue loanCoin 100 <> adaValue 2)
  , payToPubKey lender (adaValue 2 <> getLNftVal 1 getLenderNftCs utxo)
  ]

getTxOutReturn2 :: PubKeyHash -> TokenName -> TxOutRef -> Tx
getTxOutReturn2 borrower dat oref = addMintRedeemer getBorrowerNftPolicy rdm $
 mconcat
  [ mintValue getBorrowerNftPolicy (getBNftVal (-1) getBorrowerNftCs oref)
  , payToScript
      (Interest.typedValidator (Interest.ContractInfo getLenderNftCs))
      dat
      (fakeValue loanCoin 125 <> adaValue 2)
  , payToPubKey borrower (fakeValue collateralCoin 100 <> adaValue 3)
  ]
 where
    rdm = Redeemer (PlutusTx.toBuiltinData (0 :: Integer))

returnPartialLoanSameCs :: Run Bool
returnPartialLoanSameCs = do
  users <- setupUsers'
  let borrower = head users
      lender   = last users
      valToPay = fakeValue collateralCoin 100 <> adaValue 2 <> adaValue 1

  sp <- spend borrower valToPay
  let oref = getHeadRef sp
  let borrowerNftRef = oref
  let repayint = 20000
  let tx = createLockFundsTx2 repayint borrower oref sp 100000 0 <> getMintBorrowerNftTx borrower oref
  submitTx borrower tx
  utxos <- utxoAt $ requestAddress getSc1Params
  let [(lockRef, _)] = utxos
  let lenderNftRef = lockRef
  lockDat <- datumAt @RequestDatum lockRef
  case lockDat of
      Just dat -> do
          let mintTime = 7000
          let convertedDat        = getCollatDatumFromRequestDat dat (getAadaTokenName lenderNftRef) mintTime
              valForLenderToSpend = fakeValue loanCoin 100 <> adaValue 4
          sp <- spend lender valForLenderToSpend
          let tx = getTxIn sp dat lockRef (getAadaTokenName lenderNftRef) <> getTxOutLend2 borrower lender convertedDat lockRef
          logInfo $  "current time1: " ++ show mintTime
          tx <- validateIn (interval 2000 6000) tx
          submitTx lender tx

          -- loan return phase

          let valTmp1 = getBNftVal 1 getBorrowerNftCs borrowerNftRef <>
                        adaValue 1
              valTmp2 = fakeValue loanCoin 125 <>
                        adaValue 2


          sp1 <- spend borrower valTmp1
          sp2 <- spend borrower valTmp2

          utxos <- utxoAt $ Collateral.collateralAddress getSc2Params
          let [(lockRef, _)] = utxos

          logInfo $ "mint date: " <> show mintTime
          wait 16000

          intPayDate <- currentTime
          logInfo $ "pay date: " <> show intPayDate
          let intDat = Collateral.lenderNftTn convertedDat
              tx2 = getTxInFromCollateral [sp1, sp2] convertedDat 0 lockRef <>
                    getTxOutReturn2 borrower intDat borrowerNftRef

          tx2 <- validateIn (interval 24000 intPayDate) tx2

          submitTx lender tx2
          pure True
      Nothing -> pure False

returnPartialLoanForgedMintDate :: Run Bool
returnPartialLoanForgedMintDate = do
  users <- setupUsers
  let borrower = head users
      lender   = last users
      valToPay = fakeValue collateralCoin 100 <> adaValue 2 <> adaValue 1
  sp <- spend borrower valToPay
  let oref = getHeadRef sp
  let borrowerNftRef = oref
  let repayint = 20000
  let tx = createLockFundsTx repayint borrower oref sp 100000 0 (scriptCurrencySymbol $ OracleNft.policy "ff" "ff" "ff" "ff" "ff") <> getMintBorrowerNftTx borrower oref
  submitTx borrower tx
  utxos <- utxoAt $ requestAddress getSc1Params
  let [(lockRef, _)] = utxos
  let lenderNftRef = lockRef
  lockDat <- datumAt @RequestDatum lockRef
  case lockDat of
      Just dat -> do
          -- lender provides loan
          let mintTime = 7000
          let convertedDat        = getCollatDatumFromRequestDat dat (getAadaTokenName lenderNftRef) mintTime
              valForLenderToSpend = fakeValue loanCoin 150 <> adaValue 4
          sp <- spend lender valForLenderToSpend
          let tx = getTxIn sp dat lockRef (getAadaTokenName lenderNftRef) <> getTxOutLend borrower lender convertedDat lockRef (adaValueOf 0)
          logInfo $  "current time1: " ++ show mintTime
          tx <- validateIn (interval 2000 6000) tx
          submitTx lender tx

          -- loan return phase
          let interestAmount = 5

          let valTmp1 = getBNftVal 1 getBorrowerNftCs borrowerNftRef <>
                        adaValue 1
              valTmp2 = fakeValue loanCoin 150 <>
                        adaValue 1
              valTmp3 = fakeValue interestCoin interestAmount <>
                        adaValue 1

          sp1 <- spend borrower valTmp1
          sp2 <- spend borrower valTmp2
          sp3 <- spend borrower valTmp3

          utxos <- utxoAt $ Collateral.collateralAddress getSc2Params
          let [(lockRef, _)] = utxos
          let intDat = Collateral.lenderNftTn convertedDat

          let tx2 = getTxInFromCollateral [sp1, sp2, sp3] convertedDat 2 lockRef <>
                    getTxOutReturn interestAmount borrower intDat (adaValueOf 0) borrowerNftRef
          tx2 <- validateIn (from 6000) tx2
          wait 15000
          time <- currentTime
          logInfo $  "time before repaying: " ++ show time
          submitTx lender tx2
          pure True
      Nothing -> pure False

returnPartialLoanLessThanItShoudInterestRepayed :: Run Bool
returnPartialLoanLessThanItShoudInterestRepayed = do
  users <- setupUsers
  let borrower = head users
      lender   = last users
      valToPay = fakeValue collateralCoin 100 <> adaValue 2 <> adaValue 1
  sp <- spend borrower valToPay
  let oref = getHeadRef sp
  let borrowerNftRef = oref
  let repayint = 20000
  let tx = createLockFundsTx repayint borrower oref sp 100000 0 (scriptCurrencySymbol $ OracleNft.policy "ff" "ff" "ff" "ff" "ff") <> getMintBorrowerNftTx borrower oref
  submitTx borrower tx
  utxos <- utxoAt $ requestAddress getSc1Params -- utxoAt  :: HasAddress addr => addr -> Run [(TxOutRef, TxOut)]
  let [(lockRef, _)] = utxos
  let lenderNftRef = lockRef
  lockDat <- datumAt @RequestDatum lockRef
  case lockDat of
      Just dat -> do
          -- lender provides loan
          let mintTime = 7000
          let convertedDat        = getCollatDatumFromRequestDat dat (getAadaTokenName lenderNftRef) mintTime
              valForLenderToSpend = fakeValue loanCoin 150 <> adaValue 4
          sp <- spend lender valForLenderToSpend
          let tx = getTxIn sp dat lockRef (getAadaTokenName lenderNftRef) <> getTxOutLend borrower lender convertedDat lockRef (adaValueOf 0)
          logInfo $ "repay interval: " ++ show repayint
          logInfo $ "loan provided and timenft minted time: " ++ show mintTime
          tx <- validateIn (interval 2000 6000) tx
          submitTx lender tx

          -- loan return phase
          let interestAmount = 25
          logInfo $ "Interest amount paid: " ++ show interestAmount

          let valTmp1 = getBNftVal 1 getBorrowerNftCs borrowerNftRef <>
                        adaValue 1
              valTmp2 = fakeValue loanCoin 150 <>
                        adaValue 1
              valTmp3 = fakeValue interestCoin interestAmount <>
                        adaValue 1

          wait 15000
          intPayDate <- currentTime

          sp1 <- spend borrower valTmp1
          sp2 <- spend borrower valTmp2
          sp3 <- spend borrower valTmp3

          utxos <- utxoAt $ Collateral.collateralAddress getSc2Params
          let [(lockRef, _)] = utxos
          let intDat = Collateral.lenderNftTn convertedDat

          let tx2 = getTxInFromCollateral [sp1, sp2, sp3] convertedDat 0 lockRef <>
                    getTxOutReturn interestAmount borrower intDat (adaValueOf 0) borrowerNftRef
          tx2 <- validateIn (interval 6000 intPayDate) tx2
          time <- currentTime
          logInfo $  "time before repaying: " ++ show time
          submitTx lender tx2
          pure True
      Nothing -> pure False

getOracleNftVal :: CurrencySymbol -> Integer -> Value
getOracleNftVal cs = Value.singleton cs getOracleNftTn

builtinFromValidatorHash :: ValidatorHash -> BuiltinByteString
builtinFromValidatorHash (ValidatorHash bbs) = bbs

getMintOracleNftTx :: Integer -> PubKeyHash -> PubKeyHash -> PubKeyHash -> UserSpend -> Tx
getMintOracleNftTx n pkh1 pkh2 pkh3 usp = addMintRedeemer mp rdm $
  mconcat
    [ mintValue mp (getOracleNftVal cs n)
    , payToScript Helpers.TestValidator.typedValidator
      0
      (adaValue 2 <> getOracleNftVal cs n)
    , userSpend usp
    ]
  where
    valh = validatorHash Helpers.TestValidator.validator
    mp   = OracleNft.policy getOracleNftTn pkh1 pkh2 pkh3 (builtinFromValidatorHash valh)
    cs   = scriptCurrencySymbol mp
    rdm = Redeemer (PlutusTx.toBuiltinData (0 :: Integer))

getMintOracleNftTxInvalidValHash :: PubKeyHash -> PubKeyHash -> PubKeyHash -> UserSpend -> Tx
getMintOracleNftTxInvalidValHash pkh1 pkh2 pkh3 usp = addMintRedeemer mp rdm $
  mconcat
    [ mintValue mp (getOracleNftVal cs 1)
    , payToScript Helpers.TestValidator.failValidator
      0
      (adaValue 2 <> getOracleNftVal cs 1)
    , userSpend usp
    ]
  where
    valh = validatorHash Helpers.TestValidator.validator
    mp   = OracleNft.policy getOracleNftTn pkh1 pkh2 pkh3 (builtinFromValidatorHash valh)
    cs   = scriptCurrencySymbol mp
    rdm = Redeemer (PlutusTx.toBuiltinData (0 :: Integer))

provideLoanOnTime :: Run Bool
provideLoanOnTime = do
  users <- setupUsers
  let borrower = head users
      lender   = last users
      valToPay = fakeValue collateralCoin 100 <> adaValue 2 <> adaValue 1
  sp <- spend borrower valToPay
  let oref = getHeadRef sp
  let tx = createLockFundsTx 0 borrower oref sp 100000 0 (scriptCurrencySymbol $ OracleNft.policy "ff" "ff" "ff" "ff" "ff") <> getMintBorrowerNftTx borrower oref
  submitTx borrower tx
  utxos <- utxoAt $ requestAddress getSc1Params -- utxoAt  :: HasAddress addr => addr -> Run [(TxOutRef, TxOut)]
  let [(lockRef, _)] = utxos
  let lenderNftRef = lockRef
  lockDat <- datumAt @RequestDatum lockRef
  case lockDat of
      Just dat -> do
          let mintTime = 7000
          let convertedDat        = getCollatDatumFromRequestDat dat (getAadaTokenName lenderNftRef) mintTime
              valForLenderToSpend = fakeValue loanCoin 150 <> adaValue 4
          sp <- spend lender valForLenderToSpend
          let tx = getTxIn sp dat lockRef (getAadaTokenName lenderNftRef) <> getTxOutLend borrower lender convertedDat lockRef (adaValueOf 0)
          logInfo $  "current time: " ++ show mintTime
          tx <- validateIn (interval 2000 6000) tx
          submitTx lender tx
          pure True
      Nothing -> pure False

provideLoanNotOnTime :: Run Bool
provideLoanNotOnTime = do
  users <- setupUsers
  let borrower = head users
      lender   = last users
      valToPay = fakeValue collateralCoin 100 <> adaValue 2 <> adaValue 1
  sp <- spend borrower valToPay
  let oref = getHeadRef sp
  let tx = createLockFundsTx 0 borrower oref sp 0 0 (scriptCurrencySymbol $ OracleNft.policy "ff" "ff" "ff" "ff" "ff") <> getMintBorrowerNftTx borrower oref
  submitTx borrower tx
  utxos <- utxoAt $ requestAddress getSc1Params -- utxoAt  :: HasAddress addr => addr -> Run [(TxOutRef, TxOut)]
  let [(lockRef, _)] = utxos
  let lenderNftRef = lockRef
  lockDat <- datumAt @RequestDatum lockRef
  case lockDat of
      Just dat -> do
          let mintTime = 7000
          let convertedDat        = getCollatDatumFromRequestDat dat (getAadaTokenName lenderNftRef) mintTime
              valForLenderToSpend = fakeValue loanCoin 150 <> adaValue 4
          sp <- spend lender valForLenderToSpend
          let tx = getTxIn sp dat lockRef (getAadaTokenName lenderNftRef) <> getTxOutLend borrower lender convertedDat lockRef (adaValueOf 0)
          logInfo $  "current time: " ++ show mintTime
          tx <- validateIn (interval 2000 6000) tx
          submitTx lender tx
          pure True
      Nothing -> pure False

getTxInFromInterestSc :: UserSpend -> TxOutRef -> TokenName -> Tx
getTxInFromInterestSc usp1 scriptTxOut dat =
  mconcat
  [ spendScript (Interest.typedValidator (Interest.ContractInfo getLenderNftCs)) scriptTxOut 0 dat
  , userSpend usp1
  ]

getTxOutFromInterestSc :: Integer -> PubKeyHash -> TxOutRef -> Tx
getTxOutFromInterestSc interest lender utxo = addMintRedeemer getLenderNftPolicy utxo $
 mconcat
  [ mintValue getLenderNftPolicy (getLNftVal (-1) getLenderNftCs utxo)
  , payToPubKey lender (fakeValue loanCoin 150 <> fakeValue interestCoin interest <> adaValue 4)
  ]

happyPath :: Run Bool
happyPath = do
  users <- setupUsers
  let borrower = head users
      lender   = last users
      valToPay = fakeValue collateralCoin 100 <> adaValue 2 <> adaValue 1
  sp <- spend borrower valToPay
  let oref = getHeadRef sp
  let borrowerNftRef = oref
  let tx = createLockFundsTx 0 borrower oref sp 100000 0 (scriptCurrencySymbol $ OracleNft.policy "ff" "ff" "ff" "ff" "ff") <> getMintBorrowerNftTx borrower oref
  submitTx borrower tx
  utxos <- utxoAt $ requestAddress getSc1Params
  let lockRef = fst . head $ utxos
  let lenderNftRef = lockRef
  lockDat <- datumAt @RequestDatum lockRef
  case lockDat of
      Just dat -> do
          curTime <- currentTime
          let mintTime = POSIXTime 7000
          let convertedDat        = getCollatDatumFromRequestDat dat (getAadaTokenName lenderNftRef) mintTime
              valForLenderToSpend = fakeValue loanCoin 150 <> adaValue 4

          sp <- spend lender valForLenderToSpend
          let tx = getTxIn sp dat lockRef (getAadaTokenName lenderNftRef) <> getTxOutLend borrower lender convertedDat lockRef (adaValueOf 0)
          logInfo $  "ref: " ++ show lenderNftRef
          logInfo $  "hash: " ++ show (getAadaTokenName lenderNftRef)
          logInfo $  "mint time: " ++ show mintTime
          logInfo $  "curTime time: " ++ show curTime
          tx <- validateIn (interval 2000 6000) tx

          submitTx lender tx

          -- loan return phase
          let valTmp1 = getBNftVal 1 getBorrowerNftCs borrowerNftRef <>
                        adaValue 1
              valTmp2 = fakeValue loanCoin 150 <>
                        adaValue 1
              valTmp3 = fakeValue interestCoin 50 <>
                        adaValue 1

          wait 2000
          intPayDate <- currentTime

          sp1 <- spend borrower valTmp1
          sp2 <- spend borrower valTmp2
          sp3 <- spend borrower valTmp3

          utxos <- utxoAt $ Collateral.collateralAddress getSc2Params
          let [(lockRef, _)] = utxos
          let intDat = Collateral.lenderNftTn convertedDat

          let tx2 = getTxInFromCollateral [sp1, sp2, sp3] convertedDat 0 lockRef <>
                    getTxOutReturn 50 borrower intDat (adaValueOf 0) borrowerNftRef

          logInfo $  "int pay date time: " ++ show intPayDate
          tx2 <- validateIn (interval 5000 intPayDate) tx2
          submitTx lender tx2

          -- retrieve loan and interest phase
          utxos <- utxoAt (Interest.interestAddress (Interest.ContractInfo getLenderNftCs))
          let lenderPay = adaValue 2 <> getLNftVal 1 getLenderNftCs lenderNftRef
          sp <- spend lender lenderPay
          case utxos of
            [(lockRef, _)] -> do
              let tx = getTxInFromInterestSc sp lockRef intDat <>
                       getTxOutFromInterestSc 50 lender lenderNftRef

              submitTx lender tx

              pure True
            _ -> pure False
      Nothing -> pure False

getTxInFromCollateraLiq :: UserSpend -> UserSpend -> Collateral.CollateralDatum -> Integer -> TxOutRef -> Tx
getTxInFromCollateraLiq lender1 lender2 dat rdm scriptTxOut =
  mconcat
  [ spendScript (Collateral.collateralTypedValidator getSc2Params) scriptTxOut rdm dat
  , userSpend lender1
  , userSpend lender2
  ]

getMintOracleNftTxLiq :: Integer -> PubKeyHash -> PubKeyHash -> PubKeyHash -> Tx
getMintOracleNftTxLiq n pkh1 pkh2 pkh3 =
  mconcat
    [ mintValue mp (getOracleNftVal cs n)
    , payToScript Helpers.TestValidator.typedValidator
      0
      (adaValue 2 <> getOracleNftVal cs n)
    ]
  where
    valh = validatorHash Helpers.TestValidator.validator
    mp   = OracleNft.policy getOracleNftTn pkh1 pkh2 pkh3 (builtinFromValidatorHash valh)
    cs   = scriptCurrencySymbol mp

getTxOutLiquidate :: PubKeyHash -> TxOutRef -> Tx
getTxOutLiquidate lender utxo =
 mconcat
  [ mintValue getLenderNftPolicy (getLNftVal (-1) getLenderNftCs utxo)
  , payToPubKey lender (fakeValue collateralCoin 100 <> adaValue 2)
  ]

liquidateBorrower :: Run Bool
liquidateBorrower = do
  -- setup
  logInfo "setup"
  users1 <- setupSimpleNUsers 3
  users2 <- setupUsers
  let borrower = head users2
      lender   = last users2

  let [oracle1, oracle2, oracle3] = users1

  -- create loan request phase
  logInfo "create loan request"
  let valToPay = fakeValue collateralCoin 100 <> adaValue 2 <> adaValue 1
  sp <- spend borrower valToPay
  let oref = getHeadRef sp
  let valh = validatorHash Helpers.TestValidator.validator
      omp  = OracleNft.policy getOracleNftTn oracle1 oracle2 oracle3 (builtinFromValidatorHash valh)
      ordm = Redeemer (PlutusTx.toBuiltinData (0 :: Integer))

  let tx = createLockFundsTx 0 borrower oref sp 100000 0 (scriptCurrencySymbol omp) <> getMintBorrowerNftTx borrower oref
  submitTx borrower tx

  -- provide loan phase
  logInfo "provide loan phase"
  utxos <- utxoAt $ requestAddress getSc1Params
  let [(lockRef, _)] = utxos
  let lenderNftRef = lockRef
  lockDat <- datumAt @RequestDatum lockRef
  case lockDat of
      Just dat -> do
          let mintTime = 12000
          let convertedDat        = getCollatDatumFromRequestDat dat (getAadaTokenName lenderNftRef) mintTime
              valForLenderToSpend = fakeValue loanCoin 150 <> adaValue 4
          sp <- spend lender valForLenderToSpend
          let tx = getTxIn sp dat lockRef (getAadaTokenName lenderNftRef) <> getTxOutLend borrower lender convertedDat lockRef (adaValueOf 0)

          logInfo $  "current time: " ++ show mintTime
          realCurTime <- currentTime
          logInfo $ "real current time: " <> show realCurTime
          tx <- validateIn (interval 7000 11000) tx
          submitTx lender tx

          -- loan liquidate phase
          logInfo "liquidate phase"
          utxos <- utxoAt $ Collateral.collateralAddress getSc2Params
          let [(lockRef, _)] = utxos

          lenderSpend1 <- spend lender (adaValue 2)
          lenderSpend2 <- spend lender (getLNftVal 1 getLenderNftCs lenderNftRef)

          let liquidate = getTxInFromCollateraLiq lenderSpend1 lenderSpend2 convertedDat 0 lockRef <>
                          getMintOracleNftTxLiq 1 oracle1 oracle2 oracle3 <>
                          getTxOutLiquidate lender lenderNftRef


          let tx = addMintRedeemer getLenderNftPolicy lenderNftRef (addMintRedeemer omp ordm liquidate) -- 1.

          wait 2000

          time <- currentTime
          logInfo $ "current time: " ++ show time
          -- logInfo $ "debug tx: " <> show tx

          tx <- signTx oracle1 tx
          tx <- signTx oracle2 tx
          tx <- signTx oracle3 tx
          tx <- validateIn (interval 9000 99999) tx
          -- logInfo $ "debug liquidate: " <> show tx
          submitTx lender tx

          pure True
      Nothing -> pure False

lenderDosBorrower :: Run Bool
lenderDosBorrower = do
  users <- setupUsers''
  let borrower = head users
      lender   = last users
      valToPay = fakeValue collateralCoin 100 <> adaValue 2 <> adaValue 1
  sp <- spend borrower valToPay
  let oref = getHeadRef sp
  let tx = createLockFundsTx 0 borrower oref sp 100000 0 (scriptCurrencySymbol $ OracleNft.policy "ff" "ff" "ff" "ff" "ff") <> getMintBorrowerNftTx borrower oref
  submitTx borrower tx
  utxos <- utxoAt $ requestAddress getSc1Params
  let [(lockRef, _)] = utxos
  let lenderNftRef = lockRef
  lockDat <- datumAt @RequestDatum lockRef
  case lockDat of
      Just dat -> do
          let mintTime = 7000
          let convertedDat        = getCollatDatumFromRequestDat dat (getAadaTokenName lenderNftRef) mintTime
              valForLenderToSpend = fakeValue loanCoin 150 <> adaValue 4 <> generateFakeValues' lenderDosAmount

          sp <- spend lender valForLenderToSpend
          let tx = getTxIn sp dat lockRef (getAadaTokenName lenderNftRef) <> getTxOutLend borrower lender convertedDat lockRef (generateFakeValues' lenderDosAmount)
          logInfo $  "current time: " ++ show mintTime
          tx <- validateIn (interval 2000 6000) tx

          submitTx lender tx
          pure True
      Nothing -> pure False

borrowerDosLender :: Run Bool
borrowerDosLender = do
  users <- setupUsers'''
  let borrower = head users
      lender   = last users
      valToPay = fakeValue collateralCoin 100 <> adaValue 2 <> adaValue 1
  sp <- spend borrower valToPay
  let oref = getHeadRef sp
  let borrowerNftRef = oref
  let tx = createLockFundsTx 0 borrower oref sp 100000 0 (scriptCurrencySymbol $ OracleNft.policy "ff" "ff" "ff" "ff" "ff") <> getMintBorrowerNftTx borrower oref
  submitTx borrower tx
  utxos <- utxoAt $ requestAddress getSc1Params
  let [(lockRef, _)] = utxos
  let lenderNftRef = lockRef
  lockDat <- datumAt @RequestDatum lockRef
  case lockDat of
      Just dat -> do
          let mintTime = 7000
          let convertedDat        = getCollatDatumFromRequestDat dat (getAadaTokenName lenderNftRef) mintTime
              valForLenderToSpend = fakeValue loanCoin 150 <> adaValue 4

          sp <- spend lender valForLenderToSpend
          let tx = getTxIn sp dat lockRef (getAadaTokenName lenderNftRef) <> getTxOutLend borrower lender convertedDat lockRef (adaValueOf 0)
          logInfo $  "current time: " ++ show mintTime
          tx <- validateIn (interval 2000 6000) tx

          submitTx lender tx

          -- loan return phase

          let valTmp1 = getBNftVal 1 getBorrowerNftCs borrowerNftRef <>
                        adaValue 1
              valTmp2 = fakeValue loanCoin 150 <>
                        adaValue 1
              valTmp3 = fakeValue interestCoin 50 <>
                        adaValue 1 <>
                        generateFakeValues' borrowerDosAmount
                        -- adaValue 1
          wait 2000
          intPayDate <- currentTime

          sp1 <- spend borrower valTmp1
          sp2 <- spend borrower valTmp2
          sp3 <- spend borrower valTmp3

          utxos <- utxoAt $ Collateral.collateralAddress getSc2Params
          let [(lockRef, _)] = utxos
          let intDat = Collateral.lenderNftTn convertedDat

          let tx2 = getTxInFromCollateral [sp1, sp2, sp3] convertedDat 0 lockRef <>
                    getTxOutReturn 50 borrower intDat (generateFakeValues' borrowerDosAmount) borrowerNftRef

          logInfo $  "int pay date time: " ++ show intPayDate
          tx2 <- validateIn (interval 6000 intPayDate) tx2
          submitTx lender tx2
          pure True
      Nothing -> pure False