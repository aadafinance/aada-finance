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

liquidatorTests :: BchConfig -> TestTree
liquidatorTests cfg =
    testGroup ""
    [
        testNoErrors (adaValue 10_000_000 <> borrowerInitialFunds <> lenderInitialFunds) cfg "Happy path" happyPath
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

getMintBorrowerNftTx :: PubKeyHash -> TxOutRef -> Tx
getMintBorrowerNftTx pkh oref = addMintRedeemer getBorrowerNftPolicy oref $
  mconcat
    [ mintValue (AadaNft.policy False) (getBNftVal 1 cs oref)
    , payToPubKey pkh (adaValue 1 <> getBNftVal 1 cs oref)
    ]
  where
    cs  = scriptCurrencySymbol getBorrowerNftPolicy

-- getMintSafetyTokenTx :: TxOutRef -> Tx
-- getMintSafetyTokenTx utxo = addMintRedeemer getSafetyTokenMp $
--   mconcat
--     [ mintValue () ()
--     , payToPubKey PubKeyHash Value
--     ]

getSafetyTokenMp :: MintingPolicy
getSafetyTokenMp = St.policy getLenderNftCs

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

happyPath :: Run Bool
happyPath = do
  users <- setupUsers
  let borrower = head users
      lender   = last users
      valToPay = fakeValue collateralCoin 100 <> adaValue 2 <> adaValue 1
  sp <- spend borrower valToPay
  let oref = getHeadRef sp
  let borrowerNftRef = oref
  let tx = createLockFundsTx 0 borrower oref sp 100000 0 (scriptCurrencySymbol $ OracleNft.policy "ff" "ff" "ff" "ff") <> getMintBorrowerNftTx borrower oref
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

          -- 

          pure True
      Nothing -> pure False
