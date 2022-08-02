{-# LANGUAGE OverloadedStrings  #-}

import           Prelude
import           System.Environment

import           Cardano.Api
import           Cardano.Api.Shelley

import qualified Cardano.Ledger.Alonzo.Data as Alonzo
import qualified Plutus.V1.Ledger.Api       as Plutus

import qualified Data.ByteString.Short as SBS

import           LenderNft (lenderNftShortBs, lenderNft)

main :: IO ()
main = do
  let scriptnum = 0
  let scriptname = "lender.nft"
  putStrLn $ "Writing output to: " ++ scriptname
  writePlutusMintingScript scriptnum scriptname lenderNft lenderNftShortBs

writePlutusMintingScript :: Integer -> FilePath -> PlutusScript PlutusScriptV1 -> SBS.ShortByteString -> IO ()
writePlutusMintingScript scriptnum filename scriptSerial scriptSBS =
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