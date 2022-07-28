{-# LANGUAGE OverloadedStrings  #-}

module Main (main) where

import           Prelude

import           Cardano.Api
import           Cardano.Api.Shelley

import qualified Cardano.Ledger.Alonzo.Data as Alonzo
import           Plutus.V1.Ledger.Api
import           Ledger                 (validatorHash, scriptCurrencySymbol)

import qualified Data.ByteString.Short as SBS

import               Interest
import               Collateral
import               Request
import               TimeNft
import               Liquidation
import               LenderNft

getCollateralScParams :: CurrencySymbol -> Collateral.ContractInfo
getCollateralScParams cs = Collateral.ContractInfo {
        Collateral.borrower     = "B"
      , Collateral.lenderNftCs  = cs
      , Collateral.interestscvh = validatorHash (Interest.validator (Interest.ContractInfo cs))
      , Collateral.timeNft      = scriptCurrencySymbol TimeNft.policy
    }

getRequestScParams :: CurrencySymbol -> Request.ContractInfo
getRequestScParams cs = Request.ContractInfo {
        Request.borrower       = "B"
      , Request.lenderNftCs    = cs
      , Request.collateralcsvh = validatorHash $ Collateral.validator (getCollateralScParams cs)
      , Request.timeNft        = scriptCurrencySymbol TimeNft.policy
    }

main :: IO ()
main = do
  let scriptnum = 0
  writePlutusScript scriptnum "interest.plutus" (Interest.interest (Interest.ContractInfo $ scriptCurrencySymbol LenderNft.policy)) (interestShortBs (Interest.ContractInfo $ scriptCurrencySymbol LenderNft.policy))
  writePlutusScript scriptnum "collateral.plutus" (Collateral.collateral (getCollateralScParams (scriptCurrencySymbol LenderNft.policy))) (collateralShortBs (getCollateralScParams (scriptCurrencySymbol LenderNft.policy)))
  writePlutusScript scriptnum "request.plutus" (Request.request (getRequestScParams (scriptCurrencySymbol LenderNft.policy))) (requestShortBs (getRequestScParams (scriptCurrencySymbol LenderNft.policy)))
  writePlutusScript scriptnum "liquidation.plutus" Liquidation.liquidation liquidationShortBs

writePlutusScript :: Integer -> FilePath -> PlutusScript PlutusScriptV1 -> SBS.ShortByteString -> IO ()
writePlutusScript scriptnum filename scriptSerial scriptSBS =
  do
  case defaultCostModelParams of
        Just m ->
          let Alonzo.Data pData = toAlonzoData (ScriptDataNumber scriptnum)
              (logout, e) = evaluateScriptCounting Verbose m scriptSBS [pData]
          in do print ("Log output" :: String) >> print logout
                case e of
                  Left evalErr -> print ("Eval Error" :: String) >> print evalErr
                  Right exbudget -> print ("Ex Budget" :: String) >> print exbudget

        Nothing -> error "defaultCostModelParams failed"
  result <- writeFileTextEnvelope filename Nothing scriptSerial
  case result of
    Left err -> print $ displayError err
    Right () -> return ()