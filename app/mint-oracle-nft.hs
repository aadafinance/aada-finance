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
  let scriptnum = 0
  let scriptname = "oralce.nft"
  putStrLn $ "Writing output to: " ++ scriptname
  writePlutusMintingScript scriptnum scriptname oracleNft oracleNftShortBs (Plutus.PubKeyHash $ strToBbs pkh1) (Plutus.PubKeyHash $ strToBbs pkh2) (Plutus.PubKeyHash $ strToBbs pkh3) (Plutus.ValidatorHash $ strToBbs dest)

strToBbs :: String -> BuiltinByteString
strToBbs str = undefined

writePlutusMintingScript 
  :: Integer 
  -> FilePath 
  -> (Plutus.PubKeyHash -> Plutus.PubKeyHash -> Plutus.PubKeyHash -> Plutus.ValidatorHash -> PlutusScript PlutusScriptV1)
  -> (Plutus.PubKeyHash -> Plutus.PubKeyHash -> Plutus.PubKeyHash -> Plutus.ValidatorHash -> SBS.ShortByteString)
  -> Plutus.PubKeyHash
  -> Plutus.PubKeyHash
  -> Plutus.PubKeyHash
  -> Plutus.ValidatorHash
  -> IO ()
writePlutusMintingScript scriptnum filename scriptSerial scriptSBS pkh1 pkh2 pkh3 dest =
  do
  case Plutus.defaultCostModelParams of
        Just m ->
          let Alonzo.Data pData = toAlonzoData (ScriptDataNumber scriptnum)
              (logout, e) = Plutus.evaluateScriptCounting Plutus.Verbose m (scriptSBS pkh1 pkh2 pkh3 dest) [pData]
          in do print ("Log output" :: String) >> print logout
                case e of
                  Left evalErr -> print ("Eval Error" :: String) >> print evalErr
                  Right exbudget -> print ("Ex Budget" :: String) >> print exbudget
        Nothing -> error "defaultCostModelParams failed"
  result <- writeFileTextEnvelope filename Nothing $ scriptSerial pkh1 pkh2 pkh3 dest
  print ("Compiled oracle minting script with: " :: [Char])
  print $ "Public Key Hash 1: " ++ show pkh1
  print $ "Public Key Hash 2: " ++ show pkh2
  print $ "Public Key Hash 3: " ++ show pkh2
  print $ "Destination smart contract validator hash: " ++ show dest
  case result of
    Left err -> print $ displayError err
    Right () -> return ()