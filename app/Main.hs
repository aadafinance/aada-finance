import           Prelude
import           System.Environment

import           Cardano.Api
import           Cardano.Api.Shelley

import qualified Cardano.Ledger.Alonzo.Data as Alonzo
import qualified Plutus.V1.Ledger.Api       as Plutus
import           PlutusTx.Builtins.Class    as BI (stringToBuiltinByteString )

import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Char8 as SBC
import qualified Data.String           as FS

import           NFTStaking (nftstakingShortBs, nftstaking)
import           StakingPolicy (stakingPolicyShortBs, stakingPolicy)

main :: IO ()
main = do
  args <- getArgs
  let nargs = length args
  let scriptnum = if nargs > 0 then read (args!!0) else 0
  let scriptname = if nargs > 1 then args!!1 else  "result.plutus"
  let pkh = if nargs > 2 then args!!2 else "ff"
  let utxo = if nargs > 3 then args!!3 else "ff"
  putStrLn $ "Writing output to: " ++ scriptname
  case scriptnum of
    0 -> writePlutusScript scriptnum scriptname nftstaking nftstakingShortBs
    1 -> writePlutusMintingScript scriptnum scriptname stakingPolicy stakingPolicyShortBs (FS.fromString pkh) (parseUTxO utxo)
    _ -> return ()

parseUTxO :: String -> Plutus.TxOutRef
parseUTxO s =
  let
    (x, y) = span (/= '#') s
  in
    Plutus.TxOutRef (Plutus.TxId $ Plutus.getLedgerBytes $ FS.fromString x) $ read $ tail y

writePlutusMintingScript :: Integer -> FilePath -> (Plutus.TxOutRef -> Plutus.PubKeyHash -> PlutusScript PlutusScriptV1) -> (Plutus.TxOutRef -> Plutus.PubKeyHash -> SBS.ShortByteString) -> Plutus.PubKeyHash -> Plutus.TxOutRef -> IO ()
writePlutusMintingScript scriptnum filename scriptSerial scriptSBS pkh utxo =
  do
  case Plutus.defaultCostModelParams of
        Just m ->
          let Alonzo.Data pData = toAlonzoData (ScriptDataNumber scriptnum)
              (logout, e) = Plutus.evaluateScriptCounting Plutus.Verbose m (scriptSBS utxo pkh) [pData]
          in do print ("Log output" :: String) >> print logout
                case e of
                  Left evalErr -> print ("Eval Error" :: String) >> print evalErr
                  Right exbudget -> print ("Ex Budget" :: String) >> print exbudget
        Nothing -> error "defaultCostModelParams failed"
  result <- writeFileTextEnvelope filename Nothing $ scriptSerial utxo pkh 
  print $ "Compiled with pkh: " ++ (show pkh)
  print $ "Utxo to be consumed" ++ (show utxo)
  case result of
    Left err -> print $ displayError err
    Right () -> return ()

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
