{-# LANGUAGE OverloadedStrings  #-}

module Main (main) where

import           Prelude

import           Cardano.Api
import           Cardano.Api.Shelley

import qualified Cardano.Ledger.Alonzo.Data as Alonzo
import qualified Plutus.V1.Ledger.Api       as Plutus
import           Ledger                 (validatorHash, scriptCurrencySymbol)

import qualified Data.ByteString.Short as SBS

import               Interest
import               Collateral
import               Request
import               TimeNft

getCollateralScParams :: Collateral.ContractInfo
getCollateralScParams = Collateral.ContractInfo {
        Collateral.borrower     = "B"
      , Collateral.lender       = "L"
      , Collateral.interestscvh = validatorHash Interest.validator
      , Collateral.timeNft      = scriptCurrencySymbol TimeNft.policy
    }

getRequestScParams :: Request.ContractInfo
getRequestScParams = Request.ContractInfo {
        Request.borrower       = "B"
      , Request.lender         = "L"
      , Request.collateralcsvh = validatorHash $ Collateral.validator getCollateralScParams
      , Request.timeNft        = scriptCurrencySymbol TimeNft.policy
    }

main :: IO ()
main = do
  let scriptnum = 0
  writePlutusScript scriptnum "interest.plutus" Interest.interest interestShortBs
  writePlutusScript scriptnum "collateral.plutus" (Collateral.collateral getCollateralScParams) (collateralShortBs getCollateralScParams)
  writePlutusScript scriptnum "request.plutus" (Request.request getRequestScParams) (requestShortBs getRequestScParams)

writePlutusScript :: Integer -> FilePath -> PlutusScript PlutusScriptV1 -> SBS.ShortByteString -> IO ()
writePlutusScript scriptnum filename scriptSerial scriptSBS =
  do
  case Plutus.defaultCostModelParams of
        Just m ->
          let Alonzo.Data pData = toAlonzoData (ScriptDataNumber scriptnum)
              (logout, e) = Plutus.evaluateScriptCounting Plutus.Verbose m scriptSBS [pData]
          in do print ("Log output" :: String) >> print logout
                case e of
                  Left evalErr -> print ("Eval Error" :: String) >> print evalErr
                  Right exbudget -> print ("Ex Budget" :: String) >> print exbudget

        Nothing -> error "defaultCostModelParams failed"
  result <- writeFileTextEnvelope filename Nothing scriptSerial
  case result of
    Left err -> print $ displayError err
    Right () -> return ()