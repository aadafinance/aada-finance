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
import qualified OracleNft
import Plutus.Test.Model
import Ledger.Address (PaymentPubKeyHash(..))
import Ledger (validatorHash, scriptCurrencySymbol, interval)
import           Ledger.Value                as Value
import           PlutusTx
import qualified PlutusTx.Builtins.Internal as INT
import Collateral (CollateralDatum (repayinterval))
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

mintOracleNftTests :: BchConfig -> TestTree
mintOracleNftTests cfg =
  testGroup
    "Mint oracle nft tests"
    [
      testNoErrors (adaValue 10_000_000) cfg "test mint oracle nft" mintOracleNft
    , testNoErrors (adaValue 10_000_000) cfg "test mint oracle nft without one signature" (mustFail mintOracleNftShouldFail2)
    , testNoErrors (adaValue 10_000_000) cfg "test mint oracle nft without one signature" (mustFail mintOracleNftShouldFail3)
    , testNoErrors (adaValue 10_000_000) cfg "test mint oracle nft without one signature" (mustFail mintOracleNftShouldFail4)
    , testNoErrors (adaValue 10_000_000) cfg "test mint oracle nft without one signature" (mustFail mintOracleNftShouldFail4)
    , testNoErrors (adaValue 10_000_000) cfg "test mint oracle nft without one signature" (mustFail mintOracleNftShouldFail5)
    , testNoErrors (adaValue 10_000_000) cfg "test mint oracle nft without one signature" (mustFail mintOracleNftShouldFail6)
    , testNoErrors (adaValue 10_000_000) cfg "test mint oracle nft without one signature" (mustFail mintOracleNftShouldFail7)
    , testNoErrors (adaValue 10_000_000) cfg "test mint oracle nft send to wrong validator hash" (mustFail mintOracleNftShouldFail8)
    , testNoErrors (adaValue 10_000_000) cfg "test mint oracle nft mint two values" (mustFail mintOracleNftShouldFail9)
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

getSc1Params :: CurrencySymbol -> Request.ContractInfo
getSc1Params cs = Request.ContractInfo {
        Request.aadaNftCs       = cs
      , Request.collateralcsvh  = validatorHash $ Collateral.validator (getSc2Params cs)
    }

getSc2Params :: CurrencySymbol -> Collateral.ContractInfo
getSc2Params cs = Collateral.ContractInfo {
        Collateral.aadaNftCs    = cs
      , Collateral.interestscvh = validatorHash (Interest.validator (Interest.ContractInfo cs))
    }

getTestDatum :: POSIXTime -> TokenName -> CurrencySymbol -> PaymentPubKeyHash -> POSIXTime -> TokenName -> POSIXTime -> RequestDatum
getTestDatum returnt bNftTn liqNft pkh expiration ltn t = RequestDatum
  { borrowersNftTn        = bNftTn
  , borrowersPkh          = pkh
  , loan                  = assetClass (fakeCoinCs loanCoin) "loan-coin-CONYMONY"
  , loanamnt              = 150
  , interest              = assetClass (fakeCoinCs interestCoin) "interest-coin-MONY"
  , interestamnt          = 50
  , collateral            = assetClass (fakeCoinCs collateralCoin) "collateral-coin-CONY"
  , collateralamnt        = 100                    -- amount of collateral
  , repayinterval         = returnt
  , liquidateNft          = liqNft
  , collateralFactor      = 5                      -- Colalteral factor used for liquidation
  , liquidationCommission = 150                    -- How much % borrower will pay for lender when liquidated (before time passes)
  , requestExpiration     = expiration
  , lenderNftTn           = ltn
  , lendDate              = t
  }

getTestDatum2 :: POSIXTime -> TokenName -> CurrencySymbol -> PaymentPubKeyHash -> POSIXTime -> TokenName -> POSIXTime -> RequestDatum
getTestDatum2 returnt bNftTn liqNft pkh expiration ltn t = RequestDatum
  { borrowersNftTn        = bNftTn
  , borrowersPkh          = pkh
  , loan                  = assetClass (fakeCoinCs loanCoin) "loan-coin-CONYMONY"
  , loanamnt              = 100
  , interest              = assetClass (fakeCoinCs loanCoin) "loan-coin-CONYMONY"
  , interestamnt          = 50
  , collateral            = assetClass (fakeCoinCs collateralCoin) "collateral-coin-CONY"
  , collateralamnt        = 100                    -- amount of collateral
  , repayinterval         = returnt
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
          , Collateral.borrowersPkh          = borrowersPkh
          , Collateral.loan                  = loan
          , Collateral.loanamnt              = loanamnt
          , Collateral.interest              = interest
          , Collateral.interestamnt          = interestamnt
          , Collateral.collateral            = collateral
          , Collateral.collateralamnt        = 100                    -- amount of collateral
          , Collateral.repayinterval         = repayinterval
          , Collateral.liquidateNft          = liquidateNft
          , Collateral.collateralFactor      = 5                      -- Colalteral factor used for liquidation
          , Collateral.liquidationCommission = 150
          , Collateral.requestExpiration     = requestExpiration
          , Collateral.lenderNftTn           = newTn
          , Collateral.lendDate              = newMint
        }

getAadaTokenName :: TxOutRef -> TokenName
getAadaTokenName utxo = TokenName $ INT.sha2_256 (INT.consByteString (txOutRefIdx utxo) ((getTxId . txOutRefId) utxo))

createLockFundsTx :: POSIXTime -> PubKeyHash -> TxOutRef -> UserSpend -> POSIXTime -> POSIXTime -> CurrencySymbol -> Tx
createLockFundsTx t pkh oref usp expiration mintDate oracle =
    mconcat
      [ userSpend usp
      , payToScript
        (requestTypedValidator (getSc1Params (scriptCurrencySymbol AadaNft.policy)))
        (getTestDatum t (getAadaTokenName oref) oracle (PaymentPubKeyHash pkh) expiration "" mintDate)
        (fakeValue collateralCoin 100 <> adaValue 2)
      ]

getCancelRequestTx :: PubKeyHash -> Value -> RequestDatum -> TxOutRef -> TokenName -> Tx
getCancelRequestTx pkh val dat lockRef lenderTn =
    mconcat
      [ spendScript (requestTypedValidator (getSc1Params (scriptCurrencySymbol AadaNft.policy))) lockRef lenderTn dat
      , payToPubKey pkh val
      ]

getTxIn :: UserSpend -> RequestDatum -> TxOutRef -> TokenName -> Tx
getTxIn usp dat scriptTxOut lenderTn =
  mconcat
  [ spendScript (requestTypedValidator (getSc1Params (scriptCurrencySymbol AadaNft.policy))) scriptTxOut lenderTn dat
  , userSpend usp
  ]

getOracleNftTn :: TokenName
getOracleNftTn = TokenName "ff"

getLNftVal :: Integer -> CurrencySymbol -> TxOutRef -> Value
getLNftVal n cs utxo = Value.singleton cs (getAadaTokenName utxo) n

getBNftVal :: Integer -> CurrencySymbol -> TxOutRef -> Value
getBNftVal n cs utxo = Value.singleton cs (getAadaTokenName utxo) n

getMintBorrowerNftTx :: PubKeyHash -> TxOutRef -> Tx
getMintBorrowerNftTx pkh oref = addMintRedeemer AadaNft.policy oref $
  mconcat
    [ mintValue AadaNft.policy (getBNftVal 1 cs oref)
    , payToPubKey pkh (adaValue 1 <> getBNftVal 1 cs oref)
    ]
  where
    cs  = scriptCurrencySymbol AadaNft.policy

-- getCancelRequestTx :: PubKeyHash -> Value -> RequestDatum -> TxOutRef -> Tx
-- getCancelRequestTx pkh val dat lockRef =
--     mconcat
--       [ spendScript (requestTypedValidator (getSc1Params (scriptCurrencySymbol AadaNft.policy))) lockRef 0 dat
--       , payToPubKey pkh val
--       ]

getTxOutLend :: PubKeyHash -> PubKeyHash -> Collateral.CollateralDatum -> MintingPolicy -> TxOutRef -> Value -> Tx
getTxOutLend borrower lender dat nmp utxo valToScript = addMintRedeemer nmp utxo $
 mconcat
  [ mintValue nmp (getLNftVal 1 ncs utxo)
  , payToScript
      (Collateral.collateralTypedValidator (getSc2Params (scriptCurrencySymbol AadaNft.policy)))
      dat
      (fakeValue collateralCoin 100 <> adaValue 2 <> valToScript)
  , payToPubKey borrower (fakeValue loanCoin 150 <> adaValue 2)
  , payToPubKey lender (adaValue 2 <> getLNftVal 1 ncs utxo)
  ]
 where
    ncs  = scriptCurrencySymbol nmp

getTxOutReturn :: Integer -> PubKeyHash ->  TokenName -> Value -> TxOutRef  -> Tx
getTxOutReturn interest borrower dat valToInt oref = addMintRedeemer AadaNft.policy oref $
 mconcat
  [ mintValue AadaNft.policy (getBNftVal (-1) bcs oref)
  , payToScript
      (Interest.typedValidator (Interest.ContractInfo $ scriptCurrencySymbol AadaNft.policy))
      dat
      (fakeValue loanCoin 150 <> fakeValue interestCoin interest <> adaValue 2 <> valToInt)
  , payToPubKey borrower (fakeValue collateralCoin 100 <> adaValue 3)
  ]
 where
    bcs  = scriptCurrencySymbol AadaNft.policy

getTxInFromCollateral :: [UserSpend] -> Collateral.CollateralDatum -> POSIXTime -> TxOutRef -> Tx
getTxInFromCollateral usps dat rdm scriptTxOut =
  mconcat
  (spendScript (Collateral.collateralTypedValidator (getSc2Params (scriptCurrencySymbol AadaNft.policy))) scriptTxOut rdm dat : fmap userSpend usps)

getBurnBorrowerNftTx ::  PubKeyHash -> TxOutRef -> UserSpend -> Tx
getBurnBorrowerNftTx pkh oref usp = addMintRedeemer AadaNft.policy oref $
  mconcat
    [ mintValue AadaNft.policy (getBNftVal (-1) cs oref)
    , payToPubKey pkh (adaValue 1)
    , userSpend usp
    ]
  where
    cs  = scriptCurrencySymbol AadaNft.policy

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
  utxos <- utxoAt $ requestAddress (getSc1Params (scriptCurrencySymbol AadaNft.policy))
  let [(lockRef, _)] = utxos
  lockDat <- datumAt @RequestDatum lockRef
  case lockDat of
      Just dat -> do
          let valFromSc1 = fakeValue collateralCoin 100 <> adaValue 2
              valFromUsr = adaValue 1 <> getBNftVal 1 (scriptCurrencySymbol AadaNft.policy) borrowerNftRef
          sp <- spend u1 valFromUsr
          tx <- signTx u1 $ getCancelRequestTx u1 valFromSc1 dat lockRef (getAadaTokenName lockRef) <> getBurnBorrowerNftTx u1 borrowerNftRef sp
          isRight <$> sendTx tx
      Nothing -> pure False

-- Create Loan Request tx
-- >>>>>>>>>>>>>>>>                      Tx 1                        >>>>>>>>>>>>>>>>
--                                      ┌────┐      n collateral + 2 ADA     ┌───┐    
--                                      │    ├─────────────────────────────▶│SC1│    
--              n Collateral + 2 ADA    │    │           datum               └───┘    
-- Borrower ──────────────────────────▶│    │                                       
--                     datum            │    │    Borrower NFT + 1 ADA               
--                                      │ Tx ├─────────────────────────────▶ Borrower
--                                      │    │                                       
--                1 ADA (for mint)      │    │     
-- Borrower ──────────────────────────▶│    │
--                                      │    │                                       
--                                      └────┘            
-- >>>>>>>>>>>>>>>>                                                  >>>>>>>>>>>>>>>>     

-- Provide Loan tx
-- >>>>>>>>>>>>>>>>               Tx 2                >>>>>>>>>>>>>>>>
--                               ┌────┐ n collateral + Lenders NFT + Time Nft   ┌───┐    
--                               │    ├───────────────────────────────────────▶│SC2│    
--            n Loan + 2 ADA     │    │                 datum                   └───┘    
-- Lender ─────────────────────▶│    │                                                 
--                               │    │       Lenders NFT + 2 ADA                       
--            2 ADA (for mint)   │ Tx ├───────────────────────────────────────▶ Lender  
-- Lender ─────────────────────▶│    │                                                 
--                               │    │                                                 
--   ┌───┐ n Collateral + 2 ADA  │    │            Loan + 2 ADA                         
--   │SC1├─────────────────────▶│    ├───────────────────────────────────────▶ Borrower
--   └───┘        datum          └────┘            
-- >>>>>>>>>>>>>>>>                                    >>>>>>>>>>>>>>>>       

-- bchUtxos        :: !(Map TxOutRef TxOut)

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
  utxos <- utxoAt $ requestAddress (getSc1Params (scriptCurrencySymbol AadaNft.policy)) -- utxoAt  :: HasAddress addr => addr -> Run [(TxOutRef, TxOut)]
  let [(lockRef, _)] = utxos
  let lenderNftRef = lockRef
  lockDat <- datumAt @RequestDatum lockRef
  case lockDat of
      Just dat -> do
          let mintTime = 7000
          let convertedDat        = getCollatDatumFromRequestDat dat (getAadaTokenName lenderNftRef) mintTime
              valForLenderToSpend = fakeValue loanCoin 150 <> adaValue 4
          sp <- spend lender valForLenderToSpend
          let tx = getTxIn sp dat lockRef (getAadaTokenName lenderNftRef) <> getTxOutLend borrower lender convertedDat AadaNft.policy lockRef (adaValueOf 0)
          logInfo $  "current time: " ++ show mintTime
          tx <- validateIn (interval 2000 6000) tx
          submitTx lender tx

          -- loan return phase

          let valTmp1 = getBNftVal 1 (scriptCurrencySymbol AadaNft.policy) borrowerNftRef <>
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

          utxos <- utxoAt $ Collateral.collateralAddress (getSc2Params (scriptCurrencySymbol AadaNft.policy))
          let [(lockRef, _)] = utxos

          let intDat = Collateral.lenderNftTn convertedDat

          let tx2 = getTxInFromCollateral [sp1, sp2, sp3] convertedDat intPayDate lockRef <>
                    getTxOutReturn 50 borrower intDat (adaValueOf 0) borrowerNftRef

          logInfo $  "int pay date time: " ++ show intPayDate
          tx2 <- validateIn (from 6000) tx2
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
  utxos <- utxoAt $ requestAddress (getSc1Params (scriptCurrencySymbol AadaNft.policy))
  let [(lockRef, _)] = utxos
  let lenderNftRef = lockRef
  lockDat <- datumAt @RequestDatum lockRef
  case lockDat of
      Just dat -> do
          let mintTime = 7000
          let convertedDat        = getCollatDatumFromRequestDat dat (getAadaTokenName lenderNftRef) mintTime
              valForLenderToSpend = fakeValue loanCoin 150 <> adaValue 4
          sp <- spend lender valForLenderToSpend
          let tx = getTxIn sp dat lockRef (getAadaTokenName lenderNftRef) <> getTxOutLend borrower lender convertedDat AadaNft.policy lockRef (adaValueOf 0)

          logInfo $  "current time: " ++ show mintTime
          tx <- validateIn (interval 2000 6000) tx
          submitTx lender tx

          -- loan return phase

          let valTmp1 = getBNftVal 1 (scriptCurrencySymbol AadaNft.policy) borrowerNftRef <>
                        adaValue 1
              valTmp2 = fakeValue loanCoin 150 <>
                        adaValue 1
              valTmp3 = fakeValue interestCoin 25 <>
                        adaValue 1
          intPayDate <- currentTime

          sp1 <- spend borrower valTmp1
          sp2 <- spend borrower valTmp2
          sp3 <- spend borrower valTmp3

          utxos <- utxoAt $ Collateral.collateralAddress (getSc2Params (scriptCurrencySymbol AadaNft.policy))
          let [(lockRef, _)] = utxos


          let intDat = Collateral.lenderNftTn convertedDat

          let tx2 = getTxInFromCollateral [sp1, sp2, sp3] convertedDat intPayDate lockRef <>
                    getTxOutReturn 25 borrower intDat (adaValueOf 0) borrowerNftRef
          tx2 <- validateIn (from 6000) tx2
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
  let borrowerNftRef = oref
  let repayint = 20000
  let tx = createLockFundsTx repayint borrower oref sp 100000 0 (scriptCurrencySymbol $ OracleNft.policy "ff" "ff" "ff" "ff" "ff") <> getMintBorrowerNftTx borrower oref
  submitTx borrower tx
  utxos <- utxoAt $ requestAddress (getSc1Params (scriptCurrencySymbol AadaNft.policy)) -- utxoAt  :: HasAddress addr => addr -> Run [(TxOutRef, TxOut)]
  let [(lockRef, _)] = utxos
  let lenderNftRef = lockRef
  lockDat <- datumAt @RequestDatum lockRef
  case lockDat of
      Just dat -> do
          let mintTime = 7000
          let convertedDat        = getCollatDatumFromRequestDat dat (getAadaTokenName lenderNftRef) mintTime
              valForLenderToSpend = fakeValue loanCoin 150 <> adaValue 4
          sp <- spend lender valForLenderToSpend
          let tx = getTxIn sp dat lockRef (getAadaTokenName lenderNftRef) <> getTxOutLend borrower lender convertedDat AadaNft.policy lockRef (adaValueOf 0)
          logInfo $  "current time1: " ++ show mintTime
          tx <- validateIn (interval 2000 6000) tx
          submitTx lender tx

          -- loan return phase

          let valTmp1 = getBNftVal 1 (scriptCurrencySymbol AadaNft.policy) borrowerNftRef <>
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

          utxos <- utxoAt $ Collateral.collateralAddress (getSc2Params (scriptCurrencySymbol AadaNft.policy))
          let [(lockRef, _)] = utxos
          let intDat = Collateral.lenderNftTn convertedDat

          let tx2 = getTxInFromCollateral [sp1, sp2, sp3] convertedDat intPayDate lockRef <>
                                        getTxOutReturn 25 borrower intDat (adaValueOf 0) borrowerNftRef

          tx2 <- validateIn (from 6000) tx2
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
        (requestTypedValidator (getSc1Params (scriptCurrencySymbol AadaNft.policy)))
        (getTestDatum2 t (getAadaTokenName oref) (scriptCurrencySymbol $ OracleNft.policy "ff" "ff" "ff" "ff" "ff") (PaymentPubKeyHash pkh) expiration "" mintDate)
        (fakeValue collateralCoin 100 <> adaValue 2)
      ]

getTxOutLend2 :: PubKeyHash -> PubKeyHash -> Collateral.CollateralDatum -> MintingPolicy -> TxOutRef -> Tx
getTxOutLend2 borrower lender dat nmp utxo = addMintRedeemer nmp utxo $
 mconcat
  [ mintValue nmp (getLNftVal 1 ncs utxo)
  , payToScript
      (Collateral.collateralTypedValidator (getSc2Params (scriptCurrencySymbol AadaNft.policy)))
      dat
      (fakeValue collateralCoin 100 <> adaValue 2)
  , payToPubKey borrower (fakeValue loanCoin 100 <> adaValue 2)
  , payToPubKey lender (adaValue 2 <> getLNftVal 1 ncs utxo)
  ]
 where
    ncs  = scriptCurrencySymbol nmp

getTxOutReturn2 :: PubKeyHash -> TokenName -> TxOutRef -> Tx
getTxOutReturn2 borrower dat oref = addMintRedeemer AadaNft.policy rdm $
 mconcat
  [ mintValue AadaNft.policy (getBNftVal (-1) bcs oref)
  , payToScript
      (Interest.typedValidator (Interest.ContractInfo $ scriptCurrencySymbol AadaNft.policy))
      dat
      (fakeValue loanCoin 125 <> adaValue 2)
  , payToPubKey borrower (fakeValue collateralCoin 100 <> adaValue 3)
  ]
 where
    bcs  = scriptCurrencySymbol AadaNft.policy
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
  utxos <- utxoAt $ requestAddress (getSc1Params (scriptCurrencySymbol AadaNft.policy))
  let [(lockRef, _)] = utxos
  let lenderNftRef = lockRef
  lockDat <- datumAt @RequestDatum lockRef
  case lockDat of
      Just dat -> do
          let mintTime = 7000
          let convertedDat        = getCollatDatumFromRequestDat dat (getAadaTokenName lenderNftRef) mintTime
              valForLenderToSpend = fakeValue loanCoin 100 <> adaValue 4
          sp <- spend lender valForLenderToSpend
          let tx = getTxIn sp dat lockRef (getAadaTokenName lenderNftRef) <> getTxOutLend2 borrower lender convertedDat AadaNft.policy lockRef
          logInfo $  "current time1: " ++ show mintTime
          tx <- validateIn (interval 2000 6000) tx
          submitTx lender tx

          -- loan return phase

          let valTmp1 = getBNftVal 1 (scriptCurrencySymbol AadaNft.policy) borrowerNftRef <>
                        adaValue 1
              valTmp2 = fakeValue loanCoin 125 <>
                        adaValue 2


          sp1 <- spend borrower valTmp1
          sp2 <- spend borrower valTmp2

          utxos <- utxoAt $ Collateral.collateralAddress (getSc2Params (scriptCurrencySymbol AadaNft.policy))
          let [(lockRef, _)] = utxos

          logInfo $ "mint date: " <> show mintTime
          wait 16000

          intPayDate <- currentTime
          logInfo $ "pay date: " <> show intPayDate
          let intDat = Collateral.lenderNftTn convertedDat
              tx2 = getTxInFromCollateral [sp1, sp2] convertedDat intPayDate lockRef <>
                    getTxOutReturn2 borrower intDat borrowerNftRef

          tx2 <- validateIn (from 24000) tx2

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
  utxos <- utxoAt $ requestAddress (getSc1Params (scriptCurrencySymbol AadaNft.policy))
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
          let tx = getTxIn sp dat lockRef (getAadaTokenName lenderNftRef) <> getTxOutLend borrower lender convertedDat AadaNft.policy lockRef (adaValueOf 0)
          logInfo $  "current time1: " ++ show mintTime
          tx <- validateIn (interval 2000 6000) tx
          submitTx lender tx

          -- loan return phase
          let interestAmount = 5

          let valTmp1 = getBNftVal 1 (scriptCurrencySymbol AadaNft.policy) borrowerNftRef <>
                        adaValue 1
              valTmp2 = fakeValue loanCoin 150 <>
                        adaValue 1
              valTmp3 = fakeValue interestCoin interestAmount <>
                        adaValue 1

          sp1 <- spend borrower valTmp1
          sp2 <- spend borrower valTmp2
          sp3 <- spend borrower valTmp3

          utxos <- utxoAt $ Collateral.collateralAddress (getSc2Params (scriptCurrencySymbol AadaNft.policy))
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
  utxos <- utxoAt $ requestAddress (getSc1Params (scriptCurrencySymbol AadaNft.policy)) -- utxoAt  :: HasAddress addr => addr -> Run [(TxOutRef, TxOut)]
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
          let tx = getTxIn sp dat lockRef (getAadaTokenName lenderNftRef) <> getTxOutLend borrower lender convertedDat AadaNft.policy lockRef (adaValueOf 0)
          logInfo $ "repay interval: " ++ show repayint
          logInfo $ "loan provided and timenft minted time: " ++ show mintTime
          tx <- validateIn (interval 2000 6000) tx
          submitTx lender tx

          -- loan return phase
          let interestAmount = 25
          logInfo $ "Interest amount paid: " ++ show interestAmount

          let valTmp1 = getBNftVal 1 (scriptCurrencySymbol AadaNft.policy) borrowerNftRef <>
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

          utxos <- utxoAt $ Collateral.collateralAddress (getSc2Params (scriptCurrencySymbol AadaNft.policy))
          let [(lockRef, _)] = utxos
          let intDat = Collateral.lenderNftTn convertedDat

          let tx2 = getTxInFromCollateral [sp1, sp2, sp3] convertedDat intPayDate lockRef <>
                    getTxOutReturn interestAmount borrower intDat (adaValueOf 0) borrowerNftRef
          tx2 <- validateIn (from 6000) tx2
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

mintOracleNft :: Run ()
mintOracleNft = do
  users <- setupSimpleNUsers 3
  let [u1, u2, u3] = users
  sp1 <- spend u1 (adaValue 2)
  let tx = getMintOracleNftTx 1 u1 u2 u3 sp1
  tx <- signTx u1 tx
  tx <- signTx u2 tx
  tx <- signTx u3 tx
  submitTx u1 tx

mintOracleNftShouldFail2 :: Run ()
mintOracleNftShouldFail2 = do
  users <- setupSimpleNUsers 3
  let [u1, u2, u3] = users
  sp1 <- spend u1 (adaValue 2)
  let tx = getMintOracleNftTx 1 u1 u2 u3 sp1
  tx <- signTx u1 tx
  -- tx <- signTx u2 tx
  tx <- signTx u3 tx
  submitTx u1 tx

mintOracleNftShouldFail3 :: Run ()
mintOracleNftShouldFail3 = do
  users <- setupSimpleNUsers 3
  let [u1, u2, u3] = users
  sp1 <- spend u1 (adaValue 2)
  let tx = getMintOracleNftTx 1 u1 u2 u3 sp1
  tx <- signTx u1 tx
  tx <- signTx u2 tx
  -- tx <- signTx u3 tx
  submitTx u1 tx

mintOracleNftShouldFail4 :: Run ()
mintOracleNftShouldFail4 = do
  users <- setupSimpleNUsers 3
  let [u1, u2, u3] = users
  sp1 <- spend u1 (adaValue 2)
  let tx = getMintOracleNftTx 1 u1 u2 u3 sp1
  -- tx <- signTx u1 tx
  -- tx <- signTx u2 tx
  tx <- signTx u3 tx
  submitTx u1 tx

mintOracleNftShouldFail5 :: Run ()
mintOracleNftShouldFail5 = do
  users <- setupSimpleNUsers 3
  let [u1, u2, u3] = users
  sp1 <- spend u1 (adaValue 2)
  let tx = getMintOracleNftTx 1 u1 u2 u3 sp1
  tx <- signTx u1 tx
  -- tx <- signTx u2 tx
  -- tx <- signTx u3 tx
  submitTx u1 tx

mintOracleNftShouldFail6 :: Run ()
mintOracleNftShouldFail6 = do
  users <- setupSimpleNUsers 3
  let [u1, u2, u3] = users
  sp1 <- spend u1 (adaValue 2)
  let tx = getMintOracleNftTx 1 u1 u2 u3 sp1
  -- tx <- signTx u1 tx
  tx <- signTx u2 tx
  -- tx <- signTx u3 tx
  submitTx u1 tx

mintOracleNftShouldFail7 :: Run ()
mintOracleNftShouldFail7 = do
  users <- setupSimpleNUsers 3
  let [u1, u2, u3] = users
  sp1 <- spend u1 (adaValue 2)
  let tx = getMintOracleNftTx 1 u1 u2 u3 sp1
  -- tx <- signTx u1 tx
  -- tx <- signTx u2 tx
  -- tx <- signTx u3 tx
  submitTx u1 tx

mintOracleNftShouldFail8 :: Run ()
mintOracleNftShouldFail8 = do
  users <- setupSimpleNUsers 3
  let [u1, u2, u3] = users
  sp1 <- spend u1 (adaValue 2)
  let tx = getMintOracleNftTxInvalidValHash u1 u2 u3 sp1
  tx <- signTx u1 tx
  tx <- signTx u2 tx
  tx <- signTx u3 tx
  submitTx u1 tx

mintOracleNftShouldFail9 :: Run ()
mintOracleNftShouldFail9 = do
  users <- setupSimpleNUsers 3
  let [u1, u2, u3] = users
  sp1 <- spend u1 (adaValue 2)
  let tx = getMintOracleNftTx 2 u1 u2 u3 sp1
  tx <- signTx u1 tx
  tx <- signTx u2 tx
  tx <- signTx u3 tx
  submitTx u1 tx

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
  utxos <- utxoAt $ requestAddress (getSc1Params (scriptCurrencySymbol AadaNft.policy)) -- utxoAt  :: HasAddress addr => addr -> Run [(TxOutRef, TxOut)]
  let [(lockRef, _)] = utxos
  let lenderNftRef = lockRef
  lockDat <- datumAt @RequestDatum lockRef
  case lockDat of
      Just dat -> do
          let mintTime = 7000
          let convertedDat        = getCollatDatumFromRequestDat dat (getAadaTokenName lenderNftRef) mintTime
              valForLenderToSpend = fakeValue loanCoin 150 <> adaValue 4
          sp <- spend lender valForLenderToSpend
          let tx = getTxIn sp dat lockRef (getAadaTokenName lenderNftRef) <> getTxOutLend borrower lender convertedDat AadaNft.policy lockRef (adaValueOf 0)
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
  utxos <- utxoAt $ requestAddress (getSc1Params (scriptCurrencySymbol AadaNft.policy)) -- utxoAt  :: HasAddress addr => addr -> Run [(TxOutRef, TxOut)]
  let [(lockRef, _)] = utxos
  let lenderNftRef = lockRef
  lockDat <- datumAt @RequestDatum lockRef
  case lockDat of
      Just dat -> do
          let mintTime = 7000
          let convertedDat        = getCollatDatumFromRequestDat dat (getAadaTokenName lenderNftRef) mintTime
              valForLenderToSpend = fakeValue loanCoin 150 <> adaValue 4
          sp <- spend lender valForLenderToSpend
          let tx = getTxIn sp dat lockRef (getAadaTokenName lenderNftRef) <> getTxOutLend borrower lender convertedDat AadaNft.policy lockRef (adaValueOf 0)
          logInfo $  "current time: " ++ show mintTime
          tx <- validateIn (interval 2000 6000) tx
          submitTx lender tx
          pure True
      Nothing -> pure False

getTxInFromInterestSc :: UserSpend -> TxOutRef -> TokenName -> Tx
getTxInFromInterestSc usp1 scriptTxOut dat =
  mconcat
  [ spendScript (Interest.typedValidator (Interest.ContractInfo $ scriptCurrencySymbol AadaNft.policy)) scriptTxOut 0 dat
  , userSpend usp1
  ]

getTxOutFromInterestSc :: Integer -> PubKeyHash -> MintingPolicy -> CurrencySymbol -> TxOutRef -> Tx
getTxOutFromInterestSc interest lender nmp ncs utxo = addMintRedeemer nmp utxo $
 mconcat
  [ mintValue nmp (getLNftVal (-1) ncs utxo)
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
  utxos <- utxoAt $ requestAddress (getSc1Params (scriptCurrencySymbol AadaNft.policy))
  let lockRef = fst . head $ utxos
  let lenderNftRef = lockRef
  lockDat <- datumAt @RequestDatum lockRef
  case lockDat of
      Just dat -> do
          curTime <- currentTime
          let mintTime = POSIXTime 7000
          let convertedDat        = getCollatDatumFromRequestDat dat (getAadaTokenName lenderNftRef) mintTime
              valForLenderToSpend = fakeValue loanCoin 150 <> adaValue 4
              lenderCs            = scriptCurrencySymbol AadaNft.policy

          sp <- spend lender valForLenderToSpend
          let tx = getTxIn sp dat lockRef (getAadaTokenName lenderNftRef) <> getTxOutLend borrower lender convertedDat AadaNft.policy lockRef (adaValueOf 0)
          logInfo $  "ref: " ++ show lenderNftRef
          logInfo $  "hash: " ++ show (getAadaTokenName lenderNftRef)
          logInfo $  "mint time: " ++ show mintTime
          logInfo $  "curTime time: " ++ show curTime
          tx <- validateIn (interval 2000 6000) tx

          submitTx lender tx

          -- loan return phase
          let valTmp1 = getBNftVal 1 (scriptCurrencySymbol AadaNft.policy) borrowerNftRef <>
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

          utxos <- utxoAt $ Collateral.collateralAddress (getSc2Params (scriptCurrencySymbol AadaNft.policy))
          let [(lockRef, _)] = utxos
          let intDat = Collateral.lenderNftTn convertedDat

          let tx2 = getTxInFromCollateral [sp1, sp2, sp3] convertedDat intPayDate lockRef <>
                    getTxOutReturn 50 borrower intDat (adaValueOf 0) borrowerNftRef

          logInfo $  "int pay date time: " ++ show intPayDate
          tx2 <- validateIn (from 5000) tx2
          submitTx lender tx2

          -- retrieve loan and interest phase
          utxos <- utxoAt (Interest.interestAddress (Interest.ContractInfo $ scriptCurrencySymbol AadaNft.policy))
          let lenderPay = adaValue 2 <> getLNftVal 1 lenderCs lenderNftRef
          sp <- spend lender lenderPay
          case utxos of
            [(lockRef, _)] -> do
              let tx = getTxInFromInterestSc sp lockRef intDat <>
                       getTxOutFromInterestSc 50 lender AadaNft.policy lenderCs lenderNftRef

              submitTx lender tx

              pure True
            _ -> pure False
      Nothing -> pure False

getTxInFromCollateraLiq :: UserSpend -> UserSpend -> Collateral.CollateralDatum -> POSIXTime -> TxOutRef -> Tx
getTxInFromCollateraLiq lender1 lender2 dat rdm scriptTxOut =
  mconcat
  [ spendScript (Collateral.collateralTypedValidator (getSc2Params (scriptCurrencySymbol AadaNft.policy))) scriptTxOut rdm dat
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

getTxOutLiquidate :: PubKeyHash -> MintingPolicy -> CurrencySymbol -> TxOutRef -> Tx
getTxOutLiquidate lender lmp lcs utxo =
 mconcat
  [ mintValue lmp (getLNftVal (-1) lcs utxo)
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
  utxos <- utxoAt $ requestAddress (getSc1Params (scriptCurrencySymbol AadaNft.policy))
  let [(lockRef, _)] = utxos
  let lenderNftRef = lockRef
  lockDat <- datumAt @RequestDatum lockRef
  case lockDat of
      Just dat -> do
          let mintTime = 12000
          let convertedDat        = getCollatDatumFromRequestDat dat (getAadaTokenName lenderNftRef) mintTime
              valForLenderToSpend = fakeValue loanCoin 150 <> adaValue 4
          sp <- spend lender valForLenderToSpend
          let lenderCs              = scriptCurrencySymbol AadaNft.policy
              tx = getTxIn sp dat lockRef (getAadaTokenName lenderNftRef) <> getTxOutLend borrower lender convertedDat AadaNft.policy lockRef (adaValueOf 0)

          logInfo $  "current time: " ++ show mintTime
          realCurTime <- currentTime
          logInfo $ "real current time: " <> show realCurTime
          tx <- validateIn (interval 7000 11000) tx
          submitTx lender tx

          -- loan liquidate phase
          logInfo "liquidate phase"
          intPayDate <- currentTime
          utxos <- utxoAt $ Collateral.collateralAddress (getSc2Params (scriptCurrencySymbol AadaNft.policy))
          let [(lockRef, _)] = utxos

          lenderSpend1 <- spend lender (adaValue 2)
          lenderSpend2 <- spend lender (getLNftVal 1 lenderCs lenderNftRef)

          let liquidate = getTxInFromCollateraLiq lenderSpend1 lenderSpend2 convertedDat intPayDate lockRef <>
                          getMintOracleNftTxLiq 1 oracle1 oracle2 oracle3 <>
                          getTxOutLiquidate lender AadaNft.policy lenderCs lenderNftRef


          let tx = addMintRedeemer AadaNft.policy lenderNftRef (addMintRedeemer omp ordm liquidate) -- 1.

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
  utxos <- utxoAt $ requestAddress (getSc1Params (scriptCurrencySymbol AadaNft.policy))
  let [(lockRef, _)] = utxos
  let lenderNftRef = lockRef
  lockDat <- datumAt @RequestDatum lockRef
  case lockDat of
      Just dat -> do
          let mintTime = 7000
          let convertedDat        = getCollatDatumFromRequestDat dat (getAadaTokenName lenderNftRef) mintTime
              valForLenderToSpend = fakeValue loanCoin 150 <> adaValue 4 <> generateFakeValues' lenderDosAmount

          sp <- spend lender valForLenderToSpend
          let tx = getTxIn sp dat lockRef (getAadaTokenName lenderNftRef) <> getTxOutLend borrower lender convertedDat AadaNft.policy lockRef (generateFakeValues' lenderDosAmount)
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
  utxos <- utxoAt $ requestAddress (getSc1Params (scriptCurrencySymbol AadaNft.policy))
  let [(lockRef, _)] = utxos
  let lenderNftRef = lockRef
  lockDat <- datumAt @RequestDatum lockRef
  case lockDat of
      Just dat -> do
          let mintTime = 7000
          let convertedDat        = getCollatDatumFromRequestDat dat (getAadaTokenName lenderNftRef) mintTime
              valForLenderToSpend = fakeValue loanCoin 150 <> adaValue 4

          sp <- spend lender valForLenderToSpend
          let tx = getTxIn sp dat lockRef (getAadaTokenName lenderNftRef) <> getTxOutLend borrower lender convertedDat AadaNft.policy lockRef (adaValueOf 0)
          logInfo $  "current time: " ++ show mintTime
          tx <- validateIn (interval 2000 6000) tx

          submitTx lender tx

          -- loan return phase

          let valTmp1 = getBNftVal 1 (scriptCurrencySymbol AadaNft.policy) borrowerNftRef <>
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

          utxos <- utxoAt $ Collateral.collateralAddress (getSc2Params (scriptCurrencySymbol AadaNft.policy))
          let [(lockRef, _)] = utxos
          let intDat = Collateral.lenderNftTn convertedDat

          let tx2 = getTxInFromCollateral [sp1, sp2, sp3] convertedDat intPayDate lockRef <>
                    getTxOutReturn 50 borrower intDat (generateFakeValues' borrowerDosAmount) borrowerNftRef

          logInfo $  "int pay date time: " ++ show intPayDate
          tx2 <- validateIn (from 6000) tx2
          submitTx lender tx2
          pure True
      Nothing -> pure False