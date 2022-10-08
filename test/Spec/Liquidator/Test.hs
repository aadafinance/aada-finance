{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Spec.Liquidator.Test where

import Data.Either
import Prelude

import Test.Tasty

import Plutus.V1.Ledger.Api

import           Request
import qualified Collateral
import qualified Interest
import qualified AadaNft
import qualified OracleNft
import qualified Liquidator.SafetyToken as St
import qualified Liquidator.SafetyModule as Sm
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
import Liquidator.StRedeemer.StRedeemer
import Common.Utils as U
import PlutusTx.Builtins

liquidatorTests :: BchConfig -> TestTree
liquidatorTests cfg =
    testGroup ""
    [
        testNoErrors (adaValue 10_000_000 <> borrowerInitialFunds <> lenderInitialFunds) cfg "liquidate" testLiquidate
      , testNoErrors (adaValue 10_000_000 <> borrowerInitialFunds <> lenderInitialFunds) cfg "cancel liquidation request" testCancelLiquidationRq
    ]

type RepayInterval = POSIXTime
type RequestExpirationDate = POSIXTime
type LendDate = POSIXTime
type BorrowerTokenName = TokenName
type LenderTokenName = TokenName
type BorrowersAddressPkh = PubKeyHash
type LiquidationNftCs = CurrencySymbol

fakeCoinCs :: FakeCoin -> CurrencySymbol
fakeCoinCs fc = case fakeCoin fc of
  AssetClass (cs, _) -> cs

adaValue :: Integer -> Value
adaValue = singleton adaSymbol adaToken

borrowerInitialFunds :: Value
borrowerInitialFunds = fakeValue collateralCoin 100 <> fakeValue interestCoin 50 <> adaValue 100

lenderInitialFunds :: Value
lenderInitialFunds = fakeValue loanCoin 150 <> adaValue 100

collateralCoin :: FakeCoin
collateralCoin = FakeCoin "collateral-coin-CONY"

loanCoin :: FakeCoin
loanCoin = FakeCoin "loan-coin-CONYMONY"

interestCoin :: FakeCoin
interestCoin = FakeCoin "interest-coin-MONY"

setupUsers :: Run [PubKeyHash]
setupUsers = sequenceA [newUser borrowerInitialFunds, newUser lenderInitialFunds]

setupSimpleNUsers :: Int -> Run [PubKeyHash]
setupSimpleNUsers n = replicateM n $ newUser $ adaValue 1000

-- AadaNft
getLenderNftCs :: CurrencySymbol
getLenderNftCs = scriptCurrencySymbol getLenderNftPolicy

getBorrowerNftCs :: CurrencySymbol
getBorrowerNftCs = scriptCurrencySymbol getBorrowerNftPolicy

getBorrowerNftPolicy :: MintingPolicy
getBorrowerNftPolicy = AadaNft.policy False

getLenderNftPolicy :: MintingPolicy
getLenderNftPolicy = AadaNft.policy True
-- AadaNft

createLockFundsTx :: RepayInterval -> BorrowersAddressPkh -> TxOutRef -> UserSpend -> RequestExpirationDate -> LendDate -> LiquidationNftCs -> Tx
createLockFundsTx t pkh oref usp expiration mintDate oracle =
    mconcat
      [ userSpend usp
      , payToScript
        (requestTypedValidator getSc1Params)
        (getTestDatum t (getAadaTokenName oref) oracle pkh expiration "" mintDate Nothing)
        (fakeValue collateralCoin 100 <> adaValue 2)
      ]

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

liquidateCommissions :: Integer
liquidateCommissions = 1500000000000000 -- 15%

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
  , loanDuration          = returnt
  , liquidateNft          = liqNft
  , collateralFactor      = 5                      -- Colalteral factor used for liquidation
  , liquidationCommission = liquidateCommissions
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
          , Collateral.loanDuration          = loanDuration
          , Collateral.liquidateNft          = liquidateNft
          , Collateral.collateralFactor      = 5                      -- Colalteral factor used for liquidation
          , Collateral.liquidationCommission = liquidateCommissions
          , Collateral.requestExpiration     = requestExpiration
          , Collateral.lenderNftTn           = newTn
          , Collateral.lendDate              = newMint
        }

getAadaTokenName :: TxOutRef -> TokenName
getAadaTokenName utxo = TokenName $ INT.sha2_256 (INT.consByteString (txOutRefIdx utxo) ((getTxId . txOutRefId) utxo))

getOracleNftTn :: TokenName
getOracleNftTn = TokenName "ff"

getOracleNftVal :: CurrencySymbol -> Integer -> Value
getOracleNftVal cs = Value.singleton cs getOracleNftTn

getTxIn :: UserSpend -> RequestDatum -> TxOutRef -> TokenName -> Tx
getTxIn usp dat scriptTxOut lenderTn =
  mconcat
  [ spendScript (requestTypedValidator getSc1Params) scriptTxOut lenderTn dat
  , userSpend usp
  ]

getLNftVal :: Integer -> CurrencySymbol -> TxOutRef -> Value
getLNftVal n cs utxo = Value.singleton cs (getAadaTokenName utxo) n

getBNftVal :: Integer -> CurrencySymbol -> TxOutRef -> Value
getBNftVal n cs utxo = Value.singleton cs (getAadaTokenName utxo) n

getSafetyModuleAddr :: Address
getSafetyModuleAddr = Sm.safetyAddress getSafetyModuleParams

getSafetyTokenName :: TxOutRef -> TokenName
getSafetyTokenName utxo = TokenName $ U.calculateTokenNameHash' getInterestLiquidateAddr getSafetyModuleAddr utxo

getSafetyTokenValue :: TxOutRef -> Integer -> Value -- TODO Fix below
getSafetyTokenValue utxo = Value.singleton getSafetyTokenCs (getSafetyTokenName utxo)

getMintBorrowerNftTx :: PubKeyHash -> TxOutRef -> Tx
getMintBorrowerNftTx pkh oref = addMintRedeemer getBorrowerNftPolicy oref $
  mconcat
    [ mintValue (AadaNft.policy False) (getBNftVal 1 cs oref)
    , payToPubKey pkh (adaValue 1 <> getBNftVal 1 cs oref)
    ]
  where
    cs  = scriptCurrencySymbol getBorrowerNftPolicy

getSafetyModuleParams :: Sm.ContractInfo
getSafetyModuleParams = Sm.ContractInfo getLenderNftCs getCollateralAddr getInterestLiquidateAddr getSafetyTokenCs

getSafetyTokenMp :: MintingPolicy
getSafetyTokenMp = St.policy getLenderNftCs

getSafetyTokenCs :: CurrencySymbol
getSafetyTokenCs = scriptCurrencySymbol getSafetyTokenMp

getCollateralAddr :: Address
getCollateralAddr = Collateral.collateralAddress getSc2Params

getInterestLiquidateAddr :: Address
getInterestLiquidateAddr = Interest.interestAddress $ Interest.ContractInfo getSafetyTokenCs

getInterestLiquidateValidator = Interest.typedValidator $ Interest.ContractInfo getSafetyTokenCs

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

getStRedeemer :: TxOutRef -> STRedeemer
getStRedeemer utxo = STRedeemer utxo (Interest.interestAddress $ Interest.ContractInfo getSafetyTokenCs) getSafetyModuleAddr

getTxInFromCollateral :: [UserSpend] -> Collateral.CollateralDatum -> Integer -> TxOutRef -> Tx
getTxInFromCollateral usps dat rdm scriptTxOut =
  mconcat
  (spendScript (Collateral.collateralTypedValidator getSc2Params) scriptTxOut rdm dat : fmap userSpend usps)

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

getOptForLiquidationTx :: UserSpend -> TxOutRef -> Value -> PubKeyHash -> TokenName -> Tx
getOptForLiquidationTx sp safetyTokenRef lenderNftVal lender smDatum = addMintRedeemer getSafetyTokenMp (getStRedeemer safetyTokenRef) $
 mconcat
  [ mintValue getSafetyTokenMp (getSafetyTokenValue safetyTokenRef 1)
  , payToPubKey lender (getSafetyTokenValue safetyTokenRef 1 <> adaValue 2)
  , payToScript (Sm.typedValidator getSafetyModuleParams) smDatum (adaValue 2 <> lenderNftVal)
  , userSpend sp
  ]

getTxInFromCollateraLiq :: UserSpend -> Collateral.CollateralDatum -> TokenName -> Integer -> Sm.LiquidationAction -> TxOutRef -> TxOutRef -> Tx
getTxInFromCollateraLiq liquidator colatDat smDat rdm smRdm collatScriptTxOut sMscriptTxOut =
  mconcat
  [ spendScript (Collateral.collateralTypedValidator getSc2Params) collatScriptTxOut rdm colatDat
  , spendScript (Sm.typedValidator getSafetyModuleParams) sMscriptTxOut smRdm smDat
  , userSpend liquidator
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
    mp   = OracleNft.policy getOracleNftTn pkh1 pkh2 pkh3
    cs   = scriptCurrencySymbol mp

getTxOutLiquidate :: PubKeyHash -> TxOutRef -> TokenName -> Tx
getTxOutLiquidate liquidator utxo smInterestDat =
 mconcat
  [ mintValue getLenderNftPolicy (getLNftVal (-1) getLenderNftCs utxo)
  , payToPubKey liquidator (comissionsVal <> adaValue 2)
  , payToScript
      getInterestLiquidateValidator
      smInterestDat
      (remainingVal <> adaValue 2)
  ]
  where
    total = 100
    comissions = 15
    comissionsVal = fakeValue collateralCoin comissions
    remainingVal = fakeValue collateralCoin (total - comissions)

testLiquidate :: Run Bool
testLiquidate = do
  users1 <- setupSimpleNUsers 4
  users2 <- setupUsers
  let borrower = head users2
      lender   = last users2
  let [oracle1, oracle2, oracle3, liquidator] = users1

  let valToPay = fakeValue collateralCoin 100 <> adaValue 2 <> adaValue 1
  sp <- spend borrower valToPay
  let oref = getHeadRef sp
      omp  = OracleNft.policy getOracleNftTn oracle1 oracle2 oracle3
      ordm = Redeemer (PlutusTx.toBuiltinData (0 :: Integer))
  let borrowerNftRef = oref
  let tx = createLockFundsTx 0 borrower oref sp 100000 0 (scriptCurrencySymbol omp) <>
           getMintBorrowerNftTx borrower oref
  submitTx borrower tx
  utxos <- utxoAt $ requestAddress getSc1Params
  let lockRef = fst . head $ utxos
  let lenderNftRef = lockRef
  lockDat <- datumAt @RequestDatum lockRef
  case lockDat of
      Just dat -> do
          -- Provide loan transaction
          logInfo "create liquidation request phase"
          curTime <- currentTime
          let mintTime = POSIXTime 9000
          let convertedDat        = getCollatDatumFromRequestDat dat (getAadaTokenName lenderNftRef) mintTime
              valForLenderToSpend = fakeValue loanCoin 150 <> adaValue 4

          sp <- spend lender valForLenderToSpend
          let tx = getTxIn sp dat lockRef (getAadaTokenName lenderNftRef)  <>
                   getTxOutLend borrower lender convertedDat lockRef (adaValueOf 0)
  --         logInfo $  "ref: " ++ show lenderNftRef
  --         logInfo $  "hash: " ++ show (getAadaTokenName lenderNftRef)
          logInfo $  "mint time: " ++ show mintTime
          logInfo $  "curTime time: " ++ show curTime
          -- logInfo $  "test: " ++ show tx
          tx <- validateIn (interval 2000 8000) tx
          submitTx lender tx
          -- Provide loan transaction

          -- Opt for liquidation transaction, create liquidation request
          logInfo "create liquidation request phase"
          let lenderNftVal = getLNftVal 1 getLenderNftCs lenderNftRef
          sp1 <- spend lender lenderNftVal
          sp2 <- spend lender $ adaValue 4
          val <- valueAt lender
          logInfo $ "sp1 of lender: " <> show sp1
          logInfo $ "sp2 of lender: " <> show sp2
          logInfo $ "value of lender: " <> show val
          logInfo $ "lenderNftVal: " <> show lenderNftVal
          let safetyTokenRef = getHeadRef sp2
          let smDat = getSafetyTokenName safetyTokenRef
          let tx = getOptForLiquidationTx sp1 safetyTokenRef lenderNftVal lender smDat <> userSpend sp2
          submitTx lender tx
          -- Opt for liquidation transaction, create liquidation request

          -- Liquidate loan
          logInfo "liquidate phase"
          utxos <- utxoAt $ Collateral.collateralAddress getSc2Params
          let [(collatRef, _)] = utxos
          utxos <- utxoAt $ Sm.safetyAddress getSafetyModuleParams
          let [(smRef, _)] = utxos

          liquidatorSp <- spend liquidator (adaValue 2)
          let smRdm = Sm.LiquidateWithOracle
          let liquidate = getTxInFromCollateraLiq liquidatorSp convertedDat smDat 0 smRdm collatRef smRef <>
                          getMintOracleNftTxLiq 1 oracle1 oracle2 oracle3 <>
                          getTxOutLiquidate lender lenderNftRef (getSafetyTokenName safetyTokenRef)

          let tx = addMintRedeemer getLenderNftPolicy lenderNftRef (addMintRedeemer omp ordm liquidate)

          -- time <- currentTime
          -- logInfo $ "current time: " ++ show time
          -- -- logInfo $ "debug tx: " <> show tx

          tx <- signTx oracle1 tx
          tx <- signTx oracle2 tx
          tx <- signTx oracle3 tx
          tx <- validateIn (interval 9000 99999) tx
          -- logInfo $ "expected address of liquidate interest: " <> show getInterestLiquidateAddr
          -- logInfo $ "debug liquidate loan: " <> show tx
          submitTx lender tx
          -- Liquidate loan

          pure True
      Nothing -> do
        logInfo "did not found locked datum"
        pure False

getTxInFromSafetyModule :: UserSpend -> TokenName -> Sm.LiquidationAction -> TxOutRef -> Tx
getTxInFromSafetyModule lender smDat smRdm sMscriptTxOut =
  mconcat
  [ spendScript (Sm.typedValidator getSafetyModuleParams) sMscriptTxOut smRdm smDat
  , userSpend lender
  ]

getTxOutCancelLiquidationRq :: PubKeyHash -> TxOutRef -> TokenName -> Value -> Tx
getTxOutCancelLiquidationRq lender safetyTokenRef smInterestDat lenderNftVal =
  addMintRedeemer getSafetyTokenMp (getStRedeemer safetyTokenRef) $
    mconcat
      [ mintValue getSafetyTokenMp (getSafetyTokenValue safetyTokenRef (-1))
      , payToPubKey lender (adaValue 4 <> lenderNftVal)
      ]

testCancelLiquidationRq :: Run Bool
testCancelLiquidationRq = do
  users <- setupUsers
  let borrower = head users
      lender   = last users

  let valToPay = fakeValue collateralCoin 100 <> adaValue 2 <> adaValue 1
  sp <- spend borrower valToPay
  let oref = getHeadRef sp
  let borrowerNftRef = oref
  let tx = createLockFundsTx 0 borrower oref sp 100000 0 (scriptCurrencySymbol $ OracleNft.policy "ff" "ff" "ff" "ff") <>
           getMintBorrowerNftTx borrower oref
  submitTx borrower tx
  utxos <- utxoAt $ requestAddress getSc1Params
  let lockRef = fst . head $ utxos
  let lenderNftRef = lockRef
  lockDat <- datumAt @RequestDatum lockRef
  case lockDat of
      Just dat -> do
          -- Provide loan transaction
          logInfo "create liquidation request phase"
          curTime <- currentTime
          let mintTime = POSIXTime 9000
          let convertedDat        = getCollatDatumFromRequestDat dat (getAadaTokenName lenderNftRef) mintTime
              valForLenderToSpend = fakeValue loanCoin 150 <> adaValue 4

          sp <- spend lender valForLenderToSpend
          let tx = getTxIn sp dat lockRef (getAadaTokenName lenderNftRef)  <>
                   getTxOutLend borrower lender convertedDat lockRef (adaValueOf 0)
  --         logInfo $  "ref: " ++ show lenderNftRef
  --         logInfo $  "hash: " ++ show (getAadaTokenName lenderNftRef)
          logInfo $  "mint time: " ++ show mintTime
          logInfo $  "curTime time: " ++ show curTime
          -- logInfo $  "test: " ++ show tx
          tx <- validateIn (interval 2000 8000) tx
          submitTx lender tx
          -- Provide loan transaction

          -- Opt for liquidation transaction, create liquidation request
          logInfo "create liquidation request phase"
          let lenderNftVal = getLNftVal 1 getLenderNftCs lenderNftRef
          sp1 <- spend lender lenderNftVal
          sp2 <- spend lender $ adaValue 4
          val <- valueAt lender
          logInfo $ "sp1 of lender: " <> show sp1
          logInfo $ "sp2 of lender: " <> show sp2
          logInfo $ "value of lender: " <> show val
          logInfo $ "lenderNftVal: " <> show lenderNftVal
          let safetyTokenRef = getHeadRef sp2
          let smDat = getSafetyTokenName safetyTokenRef
          let tx = getOptForLiquidationTx sp1 safetyTokenRef lenderNftVal lender smDat <> userSpend sp2
          submitTx lender tx
          -- Opt for liquidation transaction, create liquidation request

          -- Cancel liquidation request
          logInfo "cancel liquidation request"
          utxos <- utxoAt $ Sm.safetyAddress getSafetyModuleParams
          let [(smRef, _)] = utxos
          lenderSp <- spend lender (adaValue 2 <> (getSafetyTokenValue safetyTokenRef 1))
          logInfo $ "debug lenderSp: " <> show lenderSp
          let smRdm = Sm.Cancel
          let cancel = getTxInFromSafetyModule lenderSp smDat smRdm smRef <>
                       getTxOutCancelLiquidationRq lender safetyTokenRef (getSafetyTokenName safetyTokenRef) lenderNftVal
          submitTx lender cancel
          -- Cancel liquidation request

          pure True
      Nothing -> do
        logInfo "did not found locked datum"
        pure False