{-# LANGUAGE OverloadedStrings  #-}

module Main (main) where

import           Prelude
import           System.Environment
import           Cardano.Api
import           Cardano.Api.Shelley

import qualified Cardano.Ledger.Alonzo.Data as Alonzo
import           Plutus.V1.Ledger.Api
import           Ledger                 (validatorHash, scriptCurrencySymbol)

import qualified Data.ByteString.Short as SBS
import qualified Data.String           as FS

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

getCollateralScParams :: Maybe StakingCredential -> Collateral.ContractInfo
getCollateralScParams stakingCredential = Collateral.ContractInfo {
        Collateral.lenderNftCs    = getLenderNftCs
      , Collateral.borrowersNftCs = getBorrowerNftCs
      , Collateral.interestSc     = Address (ScriptCredential (validatorHash (Interest.validator (Interest.ContractInfo getLenderNftCs)))) stakingCredential
    }

getRequestScParams :: Maybe StakingCredential -> Request.ContractInfo
getRequestScParams stakingCredential = Request.ContractInfo {
        Request.lenderNftCs    = getLenderNftCs
      , Request.borrowersNftCs = getBorrowerNftCs
      , Request.collateralSc   = Address (ScriptCredential (validatorHash $ Collateral.validator (getCollateralScParams stakingCredential))) stakingCredential
    }

parseStakingKey :: String -> StakingCredential
parseStakingKey = StakingHash . PubKeyCredential . PubKeyHash . getLedgerBytes . FS.fromString

main :: IO ()
main = do
  args <- getArgs
  let nargs = length args
      stakingKey = if nargs > 0 then head args else  "ff"
      parsedStakingKey = parseStakingKey stakingKey
      scriptnum = 0
  writePlutusScript scriptnum "interest.plutus" (Interest.interestScript (Interest.ContractInfo getLenderNftCs)) (interestShortBs (Interest.ContractInfo getLenderNftCs))
  writePlutusScript scriptnum "collateral.plutus" (Collateral.collateralScript (getCollateralScParams $ Just parsedStakingKey)) (collateralShortBs (getCollateralScParams $ Just parsedStakingKey))
  writePlutusScript scriptnum "request.plutus" (Request.request (getRequestScParams $ Just parsedStakingKey)) (requestShortBs (getRequestScParams $ Just parsedStakingKey))
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