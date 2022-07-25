{-# LANGUAGE OverloadedStrings  #-}

import           Prelude
import           System.Environment

import           Cardano.Api
import           Cardano.Api.Shelley

import qualified Cardano.Ledger.Alonzo.Data as Alonzo
import qualified Plutus.V1.Ledger.Api       as Plutus
import           Ledger                 (validatorHash, scriptCurrencySymbol)

import qualified Data.ByteString.Short as SBS
import qualified Data.String           as FS

import           LenderNft (lenderNftShortBs, lenderNft)
import           Collateral
import           Interest (validator)
import           TimeNft (policy)


getCollateralScParams :: Collateral.ContractInfo
getCollateralScParams = Collateral.ContractInfo {
        Collateral.borrower     = "B"
      , Collateral.lender       = "L"
      , Collateral.interestscvh = validatorHash Interest.validator
      , Collateral.timeNft      = scriptCurrencySymbol TimeNft.policy
    }

getCollateralScValHash :: Plutus.ValidatorHash
getCollateralScValHash = validatorHash $ Collateral.validator getCollateralScParams

main :: IO ()
main = do
  args <- getArgs
  let nargs = length args
  let utxo = if nargs > 0 then head args else "ff"
  let scriptnum = 0
  let scriptname = utxo ++ ".lendernft"
  putStrLn $ "Writing output to: " ++ scriptname
  writePlutusMintingScript scriptnum scriptname lenderNft lenderNftShortBs (parseUTxO utxo)

parseUTxO :: String -> Plutus.TxOutRef
parseUTxO s =
  let
    (x, y) = span (/= '#') s
  in
    Plutus.TxOutRef (Plutus.TxId $ Plutus.getLedgerBytes $ FS.fromString x) $ read $ tail y

writePlutusMintingScript :: Integer -> FilePath -> (Plutus.ValidatorHash -> Plutus.TxOutRef -> PlutusScript PlutusScriptV1) -> (Plutus.ValidatorHash -> Plutus.TxOutRef -> SBS.ShortByteString) -> Plutus.TxOutRef -> IO ()
writePlutusMintingScript scriptnum filename scriptSerial scriptSBS utxo =
  do
  case Plutus.defaultCostModelParams of
        Just m ->
          let Alonzo.Data pData = toAlonzoData (ScriptDataNumber scriptnum)
              (logout, e) = Plutus.evaluateScriptCounting Plutus.Verbose m (scriptSBS getCollateralScValHash utxo) [pData]
          in do print ("Log output" :: String) >> print logout
                case e of
                  Left evalErr -> print ("Eval Error" :: String) >> print evalErr
                  Right exbudget -> print ("Ex Budget" :: String) >> print exbudget
        Nothing -> error "defaultCostModelParams failed"
  result <- writeFileTextEnvelope filename Nothing $ scriptSerial getCollateralScValHash utxo
  print $ "Compiled with collateral smart contract validator hash: " ++ show getCollateralScValHash
  print $ "Utxo to be consumed" ++ show utxo
  case result of
    Left err -> print $ displayError err
    Right () -> return ()