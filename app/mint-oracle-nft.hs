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
import Plutus.V1.Ledger.Api (BuiltinByteString)

main :: IO ()
main = do
  args <- getArgs
  let nargs = length args
  let pkh1 = if nargs > 0 then head args else "ff"
  let pkh2 = if nargs > 1 then args !! 1 else "ff"
  let pkh3 = if nargs > 2 then args !! 2 else "ff"
  let dest = if nargs > 3 then args !! 3 else "ff"
  let oref = if nargs > 4 then args !! 4 else "ff#0"
  let tn   = if nargs > 5 then args !! 5 else "ff"
  let scriptnum = 0
  let scriptname = "oracle.nft"
  putStrLn $ "Writing output to: " ++ scriptname
  writePlutusMintingScript scriptnum scriptname oracleNft oracleNftShortBs (parseUTxO oref) (strToPkh pkh1) (strToPkh pkh2) (strToPkh pkh3) (strToValidatorHash dest) (strToTn tn)

strToTn :: String -> Plutus.TokenName
strToTn str = Plutus.TokenName $ Plutus.getLedgerBytes $ FS.fromString str

strToPkh :: String -> Plutus.PubKeyHash
strToPkh str = Plutus.PubKeyHash $ Plutus.getLedgerBytes $ FS.fromString str

strToValidatorHash :: String -> Plutus.ValidatorHash
strToValidatorHash str = Plutus.ValidatorHash $ Plutus.getLedgerBytes $ FS.fromString str

parseUTxO :: String -> Plutus.TxOutRef
parseUTxO s =
  let
    (x, y) = span (/= '#') s
  in
    Plutus.TxOutRef (Plutus.TxId $ Plutus.getLedgerBytes $ FS.fromString x) $ read $ tail y

writePlutusMintingScript
  :: Integer
  -> FilePath
  -> (Plutus.TxOutRef -> Plutus.TokenName -> Plutus.PubKeyHash -> Plutus.PubKeyHash -> Plutus.PubKeyHash -> Plutus.ValidatorHash -> PlutusScript PlutusScriptV1)
  -> (Plutus.TxOutRef -> Plutus.TokenName -> Plutus.PubKeyHash -> Plutus.PubKeyHash -> Plutus.PubKeyHash -> Plutus.ValidatorHash -> SBS.ShortByteString)
  -> Plutus.TxOutRef
  -> Plutus.PubKeyHash
  -> Plutus.PubKeyHash
  -> Plutus.PubKeyHash
  -> Plutus.ValidatorHash
  -> Plutus.TokenName
  -> IO ()
writePlutusMintingScript scriptnum filename scriptSerial scriptSBS utxo pkh1 pkh2 pkh3 dest tn =
  do
  case Plutus.defaultCostModelParams of
        Just m ->
          let Alonzo.Data pData = toAlonzoData (ScriptDataNumber scriptnum)
              (logout, e) = Plutus.evaluateScriptCounting Plutus.Verbose m (scriptSBS utxo tn pkh1 pkh2 pkh3 dest) [pData]
          in do print ("Log output" :: String) >> print logout
                case e of
                  Left evalErr -> print ("Eval Error" :: String) >> print evalErr
                  Right exbudget -> print ("Ex Budget" :: String) >> print exbudget
        Nothing -> error "defaultCostModelParams failed"
  result <- writeFileTextEnvelope filename Nothing $ scriptSerial utxo tn pkh1 pkh2 pkh3 dest
  print ("Compiled oracle minting script with: " :: [Char])
  print $ "Public Key Hash 1: " ++ show pkh1
  print $ "Public Key Hash 2: " ++ show pkh2
  print $ "Public Key Hash 3: " ++ show pkh2
  print $ "Destination smart contract validator hash: " ++ show dest
  print $ "OracleNft token name: " ++ show tn
  case result of
    Left err -> print $ displayError err
    Right () -> return ()