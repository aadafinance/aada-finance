{-# LANGUAGE OverloadedStrings  #-}

import           Prelude
import           System.Environment

import           Cardano.Api
import           Cardano.Api.Shelley

import qualified Cardano.Ledger.Alonzo.Data as Alonzo
import qualified Plutus.V1.Ledger.Api       as Plutus

import qualified Data.ByteString.Short as SBS
import qualified Data.String           as FS

import           OracleNft (oracleNftShortBs, oracleNft)

main :: IO ()
main = do
  args <- getArgs
  let nargs = length args
  let pkh1 = if nargs > 0 then head args else "ff"
  let pkh2 = if nargs > 1 then args !! 1 else "ff"
  let pkh3 = if nargs > 2 then args !! 2 else "ff"
  let tn   = if nargs > 3 then args !! 3 else "ff"
  let scriptnum = 0
  let scriptname = "oracle.nft"
  putStrLn $ "Writing output to: " ++ scriptname
  writePlutusMintingScript scriptnum scriptname oracleNft oracleNftShortBs (strToPkh pkh1) (strToPkh pkh2) (strToPkh pkh3) (strToTn tn)

strToTn :: String -> Plutus.TokenName
strToTn str = Plutus.TokenName $ Plutus.getLedgerBytes $ FS.fromString str

strToPkh :: String -> Plutus.PubKeyHash
strToPkh str = Plutus.PubKeyHash $ Plutus.getLedgerBytes $ FS.fromString str

writePlutusMintingScript
  :: Integer
  -> FilePath
  -> (Plutus.TokenName -> Plutus.PubKeyHash -> Plutus.PubKeyHash -> Plutus.PubKeyHash -> PlutusScript PlutusScriptV1)
  -> (Plutus.TokenName -> Plutus.PubKeyHash -> Plutus.PubKeyHash -> Plutus.PubKeyHash -> SBS.ShortByteString)
  -> Plutus.PubKeyHash
  -> Plutus.PubKeyHash
  -> Plutus.PubKeyHash
  -> Plutus.TokenName
  -> IO ()
writePlutusMintingScript scriptnum filename scriptSerial scriptSBS pkh1 pkh2 pkh3 tn =
  do
  case Plutus.defaultCostModelParams of
        Just m ->
          let Alonzo.Data pData = toAlonzoData (ScriptDataNumber scriptnum)
              (logout, e) = Plutus.evaluateScriptCounting Plutus.Verbose m (scriptSBS tn pkh1 pkh2 pkh3) [pData]
          in do print ("Log output" :: String) >> print logout
                case e of
                  Left evalErr -> print ("Eval Error" :: String) >> print evalErr
                  Right exbudget -> print ("Ex Budget" :: String) >> print exbudget
        Nothing -> error "defaultCostModelParams failed"
  result <- writeFileTextEnvelope filename Nothing $ scriptSerial tn pkh1 pkh2 pkh3
  print ("Compiled oracle minting script with: " :: [Char])
  print $ "Public Key Hash 1: " ++ show pkh1
  print $ "Public Key Hash 2: " ++ show pkh2
  print $ "Public Key Hash 3: " ++ show pkh2
  print $ "OracleNft token name: " ++ show tn
  case result of
    Left err -> print $ displayError err
    Right () -> return ()