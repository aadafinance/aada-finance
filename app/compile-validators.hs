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
import               Liquidation
import               AadaNft

getLenderNftPolicy :: MintingPolicy
getLenderNftPolicy = AadaNft.policy True

getBorrowerNftPolicy :: MintingPolicy
getBorrowerNftPolicy = AadaNft.policy False

getLenderNftCs :: CurrencySymbol
getLenderNftCs = scriptCurrencySymbol getLenderNftPolicy

getBorrowerNftCs :: CurrencySymbol
getBorrowerNftCs = scriptCurrencySymbol getBorrowerNftPolicy

getCollateralScParams :: Collateral.ContractInfo
getCollateralScParams = Collateral.ContractInfo {
        Collateral.lenderNftCs    = getLenderNftCs
      , Collateral.borrowersNftCs = getBorrowerNftCs
      , Collateral.interestscvh   = validatorHash (Interest.validator (Interest.ContractInfo getLenderNftCs))
    }

getRequestScParams :: Request.ContractInfo
getRequestScParams = Request.ContractInfo {
        Request.lenderNftCs    = getLenderNftCs
      , Request.borrowersNftCs = getBorrowerNftCs
      , Request.collateralcsvh = validatorHash $ Collateral.validator getCollateralScParams
    }

main :: IO ()
main = do
  let scriptnum = 0
  writePlutusScript scriptnum "interest.plutus" (Interest.interestScript (Interest.ContractInfo getLenderNftCs)) (interestShortBs (Interest.ContractInfo getLenderNftCs))
  writePlutusScript scriptnum "collateral.plutus" (Collateral.collateralScript getCollateralScParams) (collateralShortBs getCollateralScParams)
  writePlutusScript scriptnum "request.plutus" (Request.request getRequestScParams) (requestShortBs getRequestScParams)
  writePlutusScript scriptnum "liquidation.plutus" (Liquidation.liquidation $ Liquidation.ContractInfo getBorrowerNftCs) (liquidationShortBs  $ Liquidation.ContractInfo getBorrowerNftCs)

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