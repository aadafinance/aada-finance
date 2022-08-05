{-# LANGUAGE OverloadedStrings  #-}

import           Prelude
import           System.Environment

import           Cardano.Api
import           Cardano.Api.Shelley

import qualified Cardano.Ledger.Alonzo.Data as Alonzo
import qualified Plutus.V1.Ledger.Api       as Plutus
import qualified PlutusTx.Builtins.Internal as PTX

import qualified Data.ByteString.Short as SBS
import qualified Data.String           as FS
import Text.Hex (encodeHex)
import Ledger.Value (TokenName(unTokenName))

import Options.Applicative
import Data.Semigroup ((<>))
import Control.Monad

import           AadaNft (aadaNftShortBs, aadaNft)
import Spec.Test

writeAadaNftMintingPolicyScript :: String -> IO ()
writeAadaNftMintingPolicyScript fp = do
  putStrLn $ "Writing output to: " ++ fp
  writePlutusMintingScript 0 fp aadaNft aadaNftShortBs

defaultAadaNftFp :: FilePath
defaultAadaNftFp = "aada.nft"

data Command =
    TokenName String
  | MintingPolicy FilePath
  deriving Show

parserInfo' :: ParserInfo Command
parserInfo' = info' parser' "Generate LenderNFT minting policy or its token name"
  where
    parser' :: Parser Command
    parser' = (subparser . foldMap command')
      [ ("token-name", "Hash utxo get get Lender NFT token name", tokenNameP)
      , ("minting-policy", "Generate LenderNFT minting policy", mintingPolicyP)
      ]

    tokenNameP = TokenName <$> tokenNameArg
    mintingPolicyP = MintingPolicy <$> mintingPolicyArg

    tokenNameArg = strOption
      (mconcat
        [ help "Enter utxo to be consumed when minting LenderNFT."
        , long "utxo"
        , short 'u'
        , metavar "UTXO"
        ])

    mintingPolicyArg = strOption
      (mconcat
       [ help "Enter name of lenderNFT minting policy. Default is lender.nft"
       , long "name"
       , short 'p'
       , showDefault
       , metavar "FILEPATH"
       , value defaultAadaNftFp ])

    info' :: Parser a -> String -> ParserInfo a
    info' p desc = info
      (helper <*> p)
      (fullDesc <> progDesc desc)

    command' (cmdName, desc, parser) =
      command cmdName (info' parser desc)

main :: IO ()
main = do
  opts <- execParser parserInfo'
  case opts of
    TokenName utxo -> do
      let (PTX.BuiltinByteString byteString) = unTokenName . getLenderTokenName . parseUTxO $ utxo
      print $ show (encodeHex byteString)
    MintingPolicy fp -> do
      writeAadaNftMintingPolicyScript fp

parseUTxO :: String -> Plutus.TxOutRef
parseUTxO s =
  let
    (x, y) = span (/= '#') s
  in
    Plutus.TxOutRef (Plutus.TxId $ Plutus.getLedgerBytes $ FS.fromString x) $ read $ tail y

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