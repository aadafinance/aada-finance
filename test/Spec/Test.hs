{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TupleSections #-}
module Spec.Test where

import Data.Either
import Data.Functor (void)
import Prelude

import Test.Tasty
import Test.Tasty.HUnit

import Plutus.V1.Ledger.Api
import qualified PlutusTx.Prelude  as Plutus

import           Request
import qualified Collateral
import qualified Interest
import qualified TimeNft
import qualified BorrowerNft
import qualified LenderNft
import qualified OracleNft
import Plutus.Test.Model
import Ledger.Address (PaymentPubKeyHash(..), pubKeyHashAddress)
import Ledger (validatorHash, scriptCurrencySymbol, CurrencySymbol, POSIXTime)
import           Ledger.Value                as Value
import           PlutusTx
import Data.Monoid (Monoid(mconcat))
import qualified Data.ByteString.Char8 as BSC
import           PlutusTx.Builtins           as Builtins
import Collateral (CollateralRedeemer(CollateralRedeemer))
import qualified Data.Map.Strict  as M
import Control.Monad.State.Strict
import qualified Data.Set as S
import qualified Plutus.V1.Ledger.Tx as P
import Data.Maybe
import Helpers.TestValidator

-- TODO tidy this up, descriptions are incorrect
tests :: BchConfig -> TestTree
tests cfg =
  testGroup
    "Borrower cancels loan"
    [ testNoErrorsTrace (adaValue 10_000_000 <> borrowerInitialFunds <> lenderInitialFunds) cfg "Borrower cancels loan test" borrowerCancelsLoan
    , testNoErrorsTrace (adaValue 10_000_000 <> borrowerInitialFunds <> lenderInitialFunds) cfg "Borrower returns full interest when loan return time has passed" returnFullLoan
    , testNoErrors (adaValue 10_000_000 <> borrowerInitialFunds <> lenderInitialFunds) cfg "Borrower returns less than it should then full time has passed" (mustFail returnNotEnoughInterest)
    , testNoErrorsTrace (adaValue 10_000_000 <> borrowerInitialFunds <> lenderInitialFunds) cfg "Borrower returns loan when half the time passed returning less than full interest" returnPartialLoan
    , testNoErrors (adaValue 10_000_000) cfg "test mint oracle nft" mintOracleNft
    , testNoErrors (adaValue 10_000_000) cfg "test mint oracle nft without one signature" (mustFail mintOracleNftShouldFail2)
    , testNoErrors (adaValue 10_000_000) cfg "test mint oracle nft without one signature" (mustFail mintOracleNftShouldFail3)
    , testNoErrors (adaValue 10_000_000) cfg "test mint oracle nft without one signature" (mustFail mintOracleNftShouldFail4)
    , testNoErrors (adaValue 10_000_000) cfg "test mint oracle nft without one signature" (mustFail mintOracleNftShouldFail4)
    , testNoErrors (adaValue 10_000_000) cfg "test mint oracle nft without one signature" (mustFail mintOracleNftShouldFail5)
    , testNoErrors (adaValue 10_000_000) cfg "test mint oracle nft without one signature" (mustFail mintOracleNftShouldFail6)
    , testNoErrors (adaValue 10_000_000) cfg "test mint oracle nft without one signature" (mustFail mintOracleNftShouldFail7)
    , testNoErrors (adaValue 10_000_000) cfg "test mint oracle nft send to wrong validator hash" (mustFail mintOracleNftShouldFail8)
    , testNoErrors (adaValue 10_000_000) cfg "test mint oracle nft mint two values" (mustFail mintOracleNftShouldFail9)
    , testNoErrors (adaValue 10_000_000) cfg "test mint oracle nft try to mint same value two times" (mustFail mintOracleNftShouldFail10)
    ]

-- TODO move to utils section later
adaValue :: Integer -> Value
adaValue = singleton adaSymbol adaToken

setupUsers :: Run [PubKeyHash]
setupUsers = sequenceA [newUser borrowerInitialFunds, newUser lenderInitialFunds]

setupSimpleNUsers :: Int -> Run [PubKeyHash]
setupSimpleNUsers n = replicateM n $ newUser $ adaValue 1000

-- TODO could this be done better?
fakeCoinCs :: FakeCoin -> CurrencySymbol
fakeCoinCs fc = case fakeCoin fc of
  AssetClass (cs, _) -> cs
  _       -> "ff"

collateralCoin :: FakeCoin
collateralCoin = FakeCoin "collateral-coin-CONY"

loanCoin :: FakeCoin
loanCoin = FakeCoin "loan-coin-CONYMONY"

interestCoin :: FakeCoin
interestCoin = FakeCoin "interest-coin-MONY"

borrowerInitialFunds :: Value
borrowerInitialFunds = fakeValue collateralCoin 100 <> fakeValue interestCoin 50 <> adaValue 100

lenderInitialFunds :: Value
lenderInitialFunds = fakeValue loanCoin 150 <> adaValue 100

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

getTestDatum :: POSIXTime -> CurrencySymbol -> CurrencySymbol -> PaymentPubKeyHash -> RequestDatum
getTestDatum returnt bNftCs liqNft pkh = RequestDatum
          { borrowersNFT      = bNftCs
          , borrowersPkh      = pkh
          , loantn            = "loan-coin-CONYMONY"
          , loancs            = fakeCoinCs loanCoin
          , loanamnt          = 150
          , interesttn        = "interest-coin-MONY"
          , interestcs        = fakeCoinCs interestCoin
          , interestamnt      = 50
          , collateralcs      = fakeCoinCs collateralCoin
          , repayinterval     = returnt
          , liquidateNft      = liqNft
        }

getCollatDatumFronRequestDat :: RequestDatum -> Collateral.CollateralDatum
getCollatDatumFronRequestDat rqDat@RequestDatum{..} = Collateral.CollateralDatum
          {  Collateral.borrowersNFT      = borrowersNFT
          ,  Collateral.borrowersPkh      = borrowersPkh
          ,  Collateral.loantn            = loantn
          ,  Collateral.loancs            = loancs
          ,  Collateral.loanamnt          = loanamnt
          ,  Collateral.interesttn        = interesttn
          ,  Collateral.interestcs        = interestcs
          ,  Collateral.interestamnt      = interestamnt
          ,  Collateral.collateralcs      = collateralcs
          ,  Collateral.repayinterval     = repayinterval
          ,  Collateral.liquidateNft      = liquidateNft
        }

getBNftCs :: TxOutRef -> CurrencySymbol
getBNftCs = scriptCurrencySymbol . BorrowerNft.policy

createLockFundsTx :: POSIXTime -> PubKeyHash -> TxOutRef -> UserSpend -> Value -> Tx
createLockFundsTx t pkh oref usp v =
    mconcat
      [ userSpend usp
      , payToScript
        (requestTypedValidator getSc1Params)
        (getTestDatum t (getBNftCs oref) (scriptCurrencySymbol $ OracleNft.policy (TxOutRef "ff" 0) "ff" "ff" "ff" "ff" "ff") (PaymentPubKeyHash pkh))
        (fakeValue collateralCoin 100 <> adaValue 2)
      ]

getCancelRequestTx :: PubKeyHash -> Value -> RequestDatum -> TxOutRef -> Tx
getCancelRequestTx pkh val dat lockRef =
    mconcat
      [ spendScript (requestTypedValidator getSc1Params) lockRef 0 dat
      , payToPubKey pkh val
      ]

getTxIn :: UserSpend -> RequestDatum -> TxOutRef -> Tx
getTxIn usp dat scriptTxOut =
  mconcat
  [ spendScript (requestTypedValidator getSc1Params) scriptTxOut 0 dat
  , userSpend usp
  ]

getTimeNftTn :: POSIXTime -> TokenName
getTimeNftTn n = TokenName . toBuiltin . BSC.pack $ show (getPOSIXTime n)

getOracleNftTn :: TokenName
getOracleNftTn = TokenName "ff"

getTNftVal :: POSIXTime -> Integer -> CurrencySymbol -> Value
getTNftVal dl n cs = Value.singleton cs (getTimeNftTn dl) n

getLNftVal :: Integer -> CurrencySymbol -> Value
getLNftVal n cs = Value.singleton cs "L" n

getBNftVal :: Integer -> CurrencySymbol -> Value
getBNftVal n cs = Value.singleton cs "B" n

getMintBorrowerNftTx :: PubKeyHash -> TxOutRef -> Tx
getMintBorrowerNftTx pkh oref = addMintRedeemer mp rdm $
  mconcat
    [ mintValue mp (getBNftVal 1 cs)
    , payToPubKey pkh (adaValue 1 <> getBNftVal 1 cs)
    ]
  where
    mp  = BorrowerNft.policy oref
    cs  = scriptCurrencySymbol mp
    rdm = Redeemer (PlutusTx.toBuiltinData (0 :: Integer))

getTimeNftPolicy :: MintingPolicy
getTimeNftPolicy = TimeNft.policy

getTimeNftCurrencySymbol :: CurrencySymbol
getTimeNftCurrencySymbol = scriptCurrencySymbol getTimeNftPolicy

getTxOutLend :: PubKeyHash -> POSIXTime -> PubKeyHash -> Collateral.CollateralDatum -> MintingPolicy -> Tx
getTxOutLend borrower dl lender dat nmp = addMintRedeemer nmp nrdm $ addMintRedeemer getTimeNftPolicy dl $
 mconcat
  [ mintValue getTimeNftPolicy (getTNftVal dl 1 getTimeNftCurrencySymbol)
  , mintValue nmp (getLNftVal 2 ncs)
  , payToScript
      (Collateral.collateralTypedValidator getSc2Params)
      dat
      (fakeValue collateralCoin 100 <> adaValue 2 <> getLNftVal 1 ncs <> getTNftVal dl 1 getTimeNftCurrencySymbol)
  , payToPubKey borrower (fakeValue loanCoin 150 <> adaValue 2)
  , payToPubKey lender (adaValue 2 <> getLNftVal 1 ncs)
  ]
 where
    ncs  = scriptCurrencySymbol nmp
    nrdm = Redeemer (PlutusTx.toBuiltinData (0 :: Integer))

getTxOutReturn :: Integer -> PubKeyHash -> POSIXTime -> MintingPolicy -> CurrencySymbol -> Tx
getTxOutReturn interest borrower dl bmp ncs = addMintRedeemer bmp rdm $ addMintRedeemer getTimeNftPolicy dl $
 mconcat
  [ mintValue getTimeNftPolicy (getTNftVal dl (-1) getTimeNftCurrencySymbol)
  , mintValue bmp (getBNftVal (-1) bcs)
  , payToScript
      Interest.typedValidator
      0
      (fakeValue loanCoin 150 <> fakeValue interestCoin interest <> adaValue 2 <> getLNftVal 1 ncs)
  , payToPubKey borrower (fakeValue collateralCoin 100 <> adaValue 3)
  ]
 where
    bcs  = scriptCurrencySymbol bmp
    rdm = Redeemer (PlutusTx.toBuiltinData (0 :: Integer))

getTxInFromLend :: UserSpend -> UserSpend -> UserSpend -> Collateral.CollateralDatum -> CollateralRedeemer -> TxOutRef -> Tx
getTxInFromLend usp1 usp2 usp3 dat rdm scriptTxOut =
  mconcat
  [ spendScript (Collateral.collateralTypedValidator getSc2Params) scriptTxOut rdm dat
  , userSpend usp1
  , userSpend usp2
  , userSpend usp3
  ]

getBurnBorrowerNftTx ::  PubKeyHash -> TxOutRef -> UserSpend -> Tx
getBurnBorrowerNftTx pkh oref usp = addMintRedeemer mp rdm $
  mconcat
    [ mintValue mp (getBNftVal (-1) cs)
    , payToPubKey pkh (adaValue 1)
    , userSpend usp
    ]
  where
    mp  = BorrowerNft.policy oref
    cs  = scriptCurrencySymbol mp
    rdm = Redeemer (PlutusTx.toBuiltinData (0 :: Integer))

borrowerCancelsLoan :: Run Bool
borrowerCancelsLoan = do
  users <- setupUsers
  let u1       = head users
      valToPay = fakeValue collateralCoin 100 <> adaValue 3
  sp <- spend u1 valToPay
  let oref = getHeadRef sp
  let tx = createLockFundsTx 0 u1 oref sp valToPay <> getMintBorrowerNftTx u1 oref
  submitTx u1 tx
  utxos <- utxoAt $ requestAddress getSc1Params
  let [(lockRef, lockOut)] = utxos
  lockDat <- datumAt @RequestDatum lockRef
  case lockDat of
      Just dat -> do
          let valFromSc1 = fakeValue collateralCoin 100 <> adaValue 2
              valFromUsr = adaValue 1 <> getBNftVal 1 (scriptCurrencySymbol $ BorrowerNft.policy oref)
          sp <- spend u1 valFromUsr
          tx <- signTx u1 $ getCancelRequestTx u1 valFromSc1 dat lockRef <> getBurnBorrowerNftTx u1 oref sp
          isRight <$> sendTx tx
      Nothing -> pure False

-- Create request tx
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

-- Lend tx
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

mySpend :: PaymentPubKeyHash -> Value -> Run (Maybe UserSpend)
mySpend ppkh expected = do
  refs <- txOutRefAt $ pubKeyHashAddress ppkh Nothing
  let bchutxos = gets bchUtxos
  mUtxos <- fmap (\m -> mapM (\r -> (r,) <$> M.lookup r m) refs) bchutxos
  case mUtxos of
    Just utxos -> pure $ toRes $ foldl go (expected, []) utxos
    Nothing    -> pure Nothing
  where
    go (curVal, resUtxos) u@(_, out)
      | curVal `leq` mempty = (curVal, resUtxos)
      | nextVal `lt` curVal = (nextVal, u : resUtxos)
      | otherwise = (curVal, resUtxos)
      where
        outVal = txOutValue out
        nextVal = snd $ split $ curVal <> Plutus.negate outVal

    toRes (curVal, utxos)
      | curVal `leq` mempty = Just $ UserSpend (foldMap (S.singleton . toInput) utxos) (getChange utxos)
      | otherwise = Nothing

    toInput (ref, _) = P.TxIn ref (Just P.ConsumePublicKeyAddress)

    getChange utxos
      | change /= mempty = Just $ TxOut (pubKeyHashAddress ppkh Nothing) change Nothing
      | otherwise = Nothing
      where
        change = foldMap (txOutValue . snd) utxos <> Plutus.negate expected

returnFullLoan :: Run Bool
returnFullLoan = do
  users <- setupUsers
  let borrower = head users
      lender   = last users
      valToPay = fakeValue collateralCoin 100 <> adaValue 2 <> adaValue 1
  sp <- spend borrower valToPay
  let oref = getHeadRef sp
  let tx = createLockFundsTx 0 borrower oref sp valToPay <> getMintBorrowerNftTx borrower oref
  submitTx borrower tx
  utxos <- utxoAt $ requestAddress getSc1Params -- utxoAt  :: HasAddress addr => addr -> Run [(TxOutRef, TxOut)]
  let [(lockRef, lockOut)] = utxos
  lockDat <- datumAt @RequestDatum lockRef
  case lockDat of
      Just dat -> do
          mintTime <- currentTime
          let convertedDat        = getCollatDatumFronRequestDat dat
              valFromSc1          = fakeValue collateralCoin 100 <> adaValue 2
              valForLenderToSpend = fakeValue loanCoin 150 <> adaValue 4
          sp <- spend lender valForLenderToSpend
          let borrowerMintingPolicy = BorrowerNft.policy oref
              borrowerCs            = scriptCurrencySymbol borrowerMintingPolicy
              sc2valh               = validatorHash $ Collateral.validator getSc2Params
              lenderMintingPolicy   = LenderNft.policy sc2valh (getHeadRef sp)
              lenderCs              = scriptCurrencySymbol lenderMintingPolicy
          let tx = getTxIn sp dat lockRef <> getTxOutLend borrower mintTime lender convertedDat lenderMintingPolicy
          logInfo $  "current time: " ++ show mintTime
          tx <- validateIn (from 6000) tx
          wait 3000
          submitTx lender tx

          -- lend phase
          let bmp  = BorrowerNft.policy oref
              bcs  = scriptCurrencySymbol bmp
          let valForBorrowerToSpend = fakeValue loanCoin 150 <> fakeValue interestCoin 50 <> adaValue 2 <> getBNftVal 1 bcs
          let valTmp1 = getBNftVal 1 bcs <>
                        adaValue 1
              valTmp2 = fakeValue loanCoin 150 <>
                        adaValue 1
              valTmp3 = fakeValue interestCoin 50 <>
                        adaValue 1
          v <- valueAt borrower
          intPayDate <- currentTime
          bus <- utxoAt borrower

          sp1 <- spend borrower valTmp1
          sp2 <- spend borrower valTmp2
          sp3 <- spend borrower valTmp3

          utxos <- utxoAt $ Collateral.collateralAddress getSc2Params
          let [(lockRef, lockOut)] = utxos

          let tx2 = getTxInFromLend sp1 sp2 sp3 convertedDat (CollateralRedeemer mintTime intPayDate) lockRef <>
                    getTxOutReturn 50 borrower mintTime bmp lenderCs
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
  let tx = createLockFundsTx 0 borrower oref sp valToPay <> getMintBorrowerNftTx borrower oref
  submitTx borrower tx
  utxos <- utxoAt $ requestAddress getSc1Params -- utxoAt  :: HasAddress addr => addr -> Run [(TxOutRef, TxOut)]
  let [(lockRef, lockOut)] = utxos
  lockDat <- datumAt @RequestDatum lockRef
  case lockDat of
      Just dat -> do
          mintTime <- currentTime
          let convertedDat        = getCollatDatumFronRequestDat dat
              valFromSc1          = fakeValue collateralCoin 100 <> adaValue 2
              valForLenderToSpend = fakeValue loanCoin 150 <> adaValue 4
          sp <- spend lender valForLenderToSpend
          let borrowerMintingPolicy = BorrowerNft.policy oref
              borrowerCs            = scriptCurrencySymbol borrowerMintingPolicy
              sc2valh               = validatorHash $ Collateral.validator getSc2Params
              lenderMintingPolicy   = LenderNft.policy sc2valh (getHeadRef sp)
              lenderCs              = scriptCurrencySymbol lenderMintingPolicy
          let tx = getTxIn sp dat lockRef <> getTxOutLend borrower mintTime lender convertedDat lenderMintingPolicy
          logInfo $  "current time: " ++ show mintTime
          tx <- validateIn (from 6000) tx
          wait 3000
          submitTx lender tx

          -- lend phase
          let bmp  = BorrowerNft.policy oref
              bcs  = scriptCurrencySymbol bmp
          let valForBorrowerToSpend = fakeValue loanCoin 150 <> fakeValue interestCoin 25 <> adaValue 2 <> getBNftVal 1 bcs
          let valTmp1 = getBNftVal 1 bcs <>
                        adaValue 1
              valTmp2 = fakeValue loanCoin 150 <>
                        adaValue 1
              valTmp3 = fakeValue interestCoin 25 <>
                        adaValue 1
          v <- valueAt borrower
          intPayDate <- currentTime
          bus <- utxoAt borrower

          sp1 <- spend borrower valTmp1
          sp2 <- spend borrower valTmp2
          sp3 <- spend borrower valTmp3

          utxos <- utxoAt $ Collateral.collateralAddress getSc2Params
          let [(lockRef, lockOut)] = utxos

          let tx2 = getTxInFromLend sp1 sp2 sp3 convertedDat (CollateralRedeemer mintTime intPayDate) lockRef <>
                    getTxOutReturn 25 borrower mintTime bmp lenderCs
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
  let tx = createLockFundsTx 20000 borrower oref sp valToPay <> getMintBorrowerNftTx borrower oref
  submitTx borrower tx
  utxos <- utxoAt $ requestAddress getSc1Params -- utxoAt  :: HasAddress addr => addr -> Run [(TxOutRef, TxOut)]
  let [(lockRef, lockOut)] = utxos
  lockDat <- datumAt @RequestDatum lockRef
  case lockDat of
      Just dat -> do
          mintTime <- currentTime
          let convertedDat        = getCollatDatumFronRequestDat dat
              valFromSc1          = fakeValue collateralCoin 100 <> adaValue 2
              valForLenderToSpend = fakeValue loanCoin 150 <> adaValue 4
          sp <- spend lender valForLenderToSpend
          let borrowerMintingPolicy = BorrowerNft.policy oref
              borrowerCs            = scriptCurrencySymbol borrowerMintingPolicy
              sc2valh               = validatorHash $ Collateral.validator getSc2Params
              lenderMintingPolicy   = LenderNft.policy sc2valh (getHeadRef sp)
              lenderCs              = scriptCurrencySymbol lenderMintingPolicy
          let tx = getTxIn sp dat lockRef <> getTxOutLend borrower mintTime lender convertedDat lenderMintingPolicy
          logInfo $  "current time1: " ++ show mintTime
          tx <- validateIn (from 6000) tx
          wait 3000
          submitTx lender tx

          -- lend phase
          let bmp  = BorrowerNft.policy oref
              bcs  = scriptCurrencySymbol bmp
          let valForBorrowerToSpend = fakeValue loanCoin 150 <> fakeValue interestCoin 25 <> adaValue 2 <> getBNftVal 1 bcs
          let valTmp1 = getBNftVal 1 bcs <>
                        adaValue 1
              valTmp2 = fakeValue loanCoin 150 <>
                        adaValue 1
              valTmp3 = fakeValue interestCoin 25 <>
                        adaValue 1
          v <- valueAt borrower
          intPayDate <- currentTime
          bus <- utxoAt borrower

          sp1 <- spend borrower valTmp1
          sp2 <- spend borrower valTmp2
          sp3 <- spend borrower valTmp3

          utxos <- utxoAt $ Collateral.collateralAddress getSc2Params
          let [(lockRef, lockOut)] = utxos

          let tx2 = getTxInFromLend sp1 sp2 sp3 convertedDat (CollateralRedeemer mintTime intPayDate) lockRef <>
                    getTxOutReturn 25 borrower mintTime bmp lenderCs
          tx2 <- validateIn (from 6000) tx2
          time <- currentTime
          logInfo $  "time before repaying: " ++ show time
          submitTx lender tx2
          pure True
      Nothing -> pure False

-- lenderLiquidateLoan :: Run Bool
-- lenderLiquidateLoan = do
--   users <- setupUsers
--   let borrower = head users
--       lender   = tail users
--       valToPay = fakeValue collateralCoin 100 <> adaValue 3
--   sp <- spend u1 valToPay
--   let oref = getHeadRef sp
--   let tx = createLockFundsTx u1 oref sp valToPay <> getMintBorrowerNftTx u1 oref
--   submitTx u1 tx
--   utxos <- utxoAt $ requestAddress getSc1Params
--   let [(lockRef, lockOut)] = utxos
--   lockDat <- datumAt @RequestDatum lockRef
--   case lockDat of
--       Just dat -> do
--           let valFromSc1 = fakeValue collateralCoin 100 <> adaValue 2
--               valFromUsr = adaValue 1 <> getBNftVal 1 (scriptCurrencySymbol $ BorrowerNft.policy oref)
--           sp <- spend u1 valFromUsr
--           tx <- signTx u1 $ getCancelRequestTx u1 valFromSc1 dat lockRef <> getBurnBorrowerNftTx u1 oref sp
--           isRight <$> sendTx tx
--       Nothing -> pure False

getOracleNftVal :: CurrencySymbol -> Integer -> Value
getOracleNftVal cs = Value.singleton cs getOracleNftTn

getMintOracleNftTx :: TxOutRef -> Integer -> PubKeyHash -> PubKeyHash -> PubKeyHash -> UserSpend -> Tx
getMintOracleNftTx oref n pkh1 pkh2 pkh3 usp = addMintRedeemer mp rdm $
  mconcat
    [ mintValue mp (getOracleNftVal cs n)
    , payToScript Helpers.TestValidator.typedValidator
      0
      (adaValue 2 <> getOracleNftVal cs n)
    , userSpend usp
    ]
  where
    valh = validatorHash Helpers.TestValidator.validator
    mp   = OracleNft.policy oref getOracleNftTn pkh1 pkh2 pkh3 valh
    cs   = scriptCurrencySymbol mp
    rdm = Redeemer (PlutusTx.toBuiltinData (OracleNft.OracleData "ff" "ff" 1))

getMintOracleNftTxInvalidValHash :: TxOutRef -> PubKeyHash -> PubKeyHash -> PubKeyHash -> UserSpend -> Tx
getMintOracleNftTxInvalidValHash oref pkh1 pkh2 pkh3 usp = addMintRedeemer mp rdm $
  mconcat
    [ mintValue mp (getOracleNftVal cs 1)
    , payToScript Helpers.TestValidator.failValidator
      0
      (adaValue 2 <> getOracleNftVal cs 1)
    , userSpend usp
    ]
  where
    valh = validatorHash Helpers.TestValidator.validator
    mp   = OracleNft.policy oref getOracleNftTn pkh1 pkh2 pkh3 valh
    cs   = scriptCurrencySymbol mp
    rdm  = Redeemer (PlutusTx.toBuiltinData (OracleNft.OracleData "ff" "ff" 1))

mintOracleNft :: Run ()
mintOracleNft = do
  users <- setupSimpleNUsers 3
  let [u1, u2, u3] = users
  sp1 <- spend u1 (adaValue 2)
  let oref = getHeadRef sp1
  let tx = getMintOracleNftTx oref 1 u1 u2 u3 sp1
  tx <- signTx u1 tx
  tx <- signTx u2 tx
  tx <- signTx u3 tx
  submitTx u1 tx

mintOracleNftShouldFail2 :: Run ()
mintOracleNftShouldFail2 = do
  users <- setupSimpleNUsers 3
  let [u1, u2, u3] = users
  sp1 <- spend u1 (adaValue 2)
  let oref = getHeadRef sp1
  let tx = getMintOracleNftTx oref 1 u1 u2 u3 sp1
  tx <- signTx u1 tx
  -- tx <- signTx u2 tx
  tx <- signTx u3 tx
  submitTx u1 tx

mintOracleNftShouldFail3 :: Run ()
mintOracleNftShouldFail3 = do
  users <- setupSimpleNUsers 3
  let [u1, u2, u3] = users
  sp1 <- spend u1 (adaValue 2)
  let oref = getHeadRef sp1
  let tx = getMintOracleNftTx oref 1 u1 u2 u3 sp1
  tx <- signTx u1 tx
  tx <- signTx u2 tx
  -- tx <- signTx u3 tx
  submitTx u1 tx

mintOracleNftShouldFail4 :: Run ()
mintOracleNftShouldFail4 = do
  users <- setupSimpleNUsers 3
  let [u1, u2, u3] = users
  sp1 <- spend u1 (adaValue 2)
  let oref = getHeadRef sp1
  let tx = getMintOracleNftTx oref 1 u1 u2 u3 sp1
  -- tx <- signTx u1 tx
  -- tx <- signTx u2 tx
  tx <- signTx u3 tx
  submitTx u1 tx

  
mintOracleNftShouldFail5 :: Run ()
mintOracleNftShouldFail5 = do
  users <- setupSimpleNUsers 3
  let [u1, u2, u3] = users
  sp1 <- spend u1 (adaValue 2)
  let oref = getHeadRef sp1
  let tx = getMintOracleNftTx oref 1 u1 u2 u3 sp1
  tx <- signTx u1 tx
  -- tx <- signTx u2 tx
  -- tx <- signTx u3 tx
  submitTx u1 tx

  
mintOracleNftShouldFail6 :: Run ()
mintOracleNftShouldFail6 = do
  users <- setupSimpleNUsers 3
  let [u1, u2, u3] = users
  sp1 <- spend u1 (adaValue 2)
  let oref = getHeadRef sp1
  let tx = getMintOracleNftTx oref 1 u1 u2 u3 sp1
  -- tx <- signTx u1 tx
  tx <- signTx u2 tx
  -- tx <- signTx u3 tx
  submitTx u1 tx

  
mintOracleNftShouldFail7 :: Run ()
mintOracleNftShouldFail7 = do
  users <- setupSimpleNUsers 3
  let [u1, u2, u3] = users
  sp1 <- spend u1 (adaValue 2)
  let oref = getHeadRef sp1
  let tx = getMintOracleNftTx oref 1 u1 u2 u3 sp1
  -- tx <- signTx u1 tx
  -- tx <- signTx u2 tx
  -- tx <- signTx u3 tx
  submitTx u1 tx

mintOracleNftShouldFail8 :: Run ()
mintOracleNftShouldFail8 = do
  users <- setupSimpleNUsers 3
  let [u1, u2, u3] = users
  sp1 <- spend u1 (adaValue 2)
  let oref = getHeadRef sp1
  let tx = getMintOracleNftTxInvalidValHash oref u1 u2 u3 sp1
  tx <- signTx u1 tx
  tx <- signTx u2 tx
  tx <- signTx u3 tx
  submitTx u1 tx

mintOracleNftShouldFail9 :: Run ()
mintOracleNftShouldFail9 = do
  users <- setupSimpleNUsers 3
  let [u1, u2, u3] = users
  sp1 <- spend u1 (adaValue 2)
  let oref = getHeadRef sp1
  let tx = getMintOracleNftTx oref 2 u1 u2 u3 sp1
  tx <- signTx u1 tx
  tx <- signTx u2 tx
  tx <- signTx u3 tx
  submitTx u1 tx

mintOracleNftShouldFail10 :: Run ()
mintOracleNftShouldFail10 = do
  users <- setupSimpleNUsers 3
  let [u1, u2, u3] = users
  sp1 <- spend u1 (adaValue 2)
  let oref = getHeadRef sp1
  let tx = getMintOracleNftTx oref 1 u1 u2 u3 sp1
  tx <- signTx u1 tx
  tx <- signTx u2 tx
  tx <- signTx u3 tx
  submitTx u1 tx
  sp1 <- spend u1 (adaValue 2)
  let tx = getMintOracleNftTx oref 1 u1 u2 u3 sp1
  tx <- signTx u1 tx
  tx <- signTx u2 tx
  tx <- signTx u3 tx
  submitTx u1 tx
