{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications #-}

import           Prelude
import           System.Environment

import qualified Plutus.V1.Ledger.Api       as Plutus

import qualified Data.ByteString.Short as SBS
import qualified Data.String           as FS
import Ply.Core.Serialize
import Ply
import Data.Text (pack)
import           OracleNft (oracleNft)
import Plutus.V1.Ledger.Api (BuiltinByteString)

main :: IO ()
main = do
  args <- getArgs
  let nargs = length args
  let pkh1 = if nargs > 0 then head args else "ff"
  let pkh2 = if nargs > 1 then args !! 1 else "ff"
  let pkh3 = if nargs > 2 then args !! 2 else "ff"
  let dest = if nargs > 3 then args !! 3 else "ff"
  let tn   = if nargs > 4 then args !! 4 else "ff"
  let scriptname = "oracle.nft"
  putStrLn $ "Writing output to: " ++ scriptname
  writePlutusMintingScript scriptname (strToPkh pkh1) (strToPkh pkh2) (strToPkh pkh3) (strToBuiltinByteString dest) (strToTn tn)

strToTn :: String -> Plutus.TokenName
strToTn str = Plutus.TokenName $ Plutus.getLedgerBytes $ FS.fromString str

strToPkh :: String -> Plutus.PubKeyHash
strToPkh str = Plutus.PubKeyHash $ Plutus.getLedgerBytes $ FS.fromString str

strToBuiltinByteString :: String -> Plutus.BuiltinByteString
strToBuiltinByteString str = Plutus.getLedgerBytes $ FS.fromString str

writePlutusMintingScript
  :: FilePath
  -> Plutus.PubKeyHash
  -> Plutus.PubKeyHash
  -> Plutus.PubKeyHash
  -> Plutus.BuiltinByteString
  -> Plutus.TokenName
  -> IO ()
writePlutusMintingScript filename pkh1 pkh2 pkh3 dest tn = do
  let description = "oraclenft compiled with: " <> show pkh1 <> show pkh2 <> show pkh3 <> " . Destination: " <> show dest
  let typenames = [
                    (typeName @Plutus.TokenName)
                  , (typeName @Plutus.PubKeyHash)
                  , (typeName @Plutus.PubKeyHash)
                  , (typeName @Plutus.PubKeyHash)
                  , (typeName @BuiltinByteString)
                  ]
  let script = oracleNft tn pkh1 pkh2 pkh3 dest
  writeEnvelope (pack description) filename ScriptV2 MintingPolicyRole typenames script
  print ("Compiled oracle minting script with: " :: [Char])
  print $ "Public Key Hash 1: " ++ show pkh1
  print $ "Public Key Hash 2: " ++ show pkh2
  print $ "Public Key Hash 3: " ++ show pkh2
  print $ "Destination smart contract validator hash: " ++ show dest
  print $ "OracleNft token name: " ++ show tn