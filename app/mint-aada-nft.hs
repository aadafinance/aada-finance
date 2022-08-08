{-# LANGUAGE OverloadedStrings  #-}

import           Prelude
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

import           AadaNft (aadaNftShortBs, aadaNft)
import Spec.Test

writeAadaNftMintingPolicyScript :: String -> Bool -> IO ()
writeAadaNftMintingPolicyScript fp isLenderNft = do
  putStrLn $ "Writing output to: " ++ fp
  writePlutusMintingScript 0 fp (aadaNft isLenderNft) (aadaNftShortBs isLenderNft)

defaultAadaNftFp :: FilePath
defaultAadaNftFp = "aada.nft"

data CompleteCommand = CompleteCommand {
    cmd     :: Command
  , nftType :: NftType
  } deriving Show

data Command =
    TokenName String
  | MintingPolicy FilePath
  deriving Show

data NftType =
    BorrowerNft
  | LenderNft
  deriving Show

parserInfo' :: ParserInfo CompleteCommand
parserInfo' = info' parser' "Generate LenderNFT minting policy or its token name"
  where
    parser' :: Parser CompleteCommand
    parser' = (subparser . foldMap command')
      [ ("token-name", "Hash utxo get get Lender NFT token name", tokenNameP)
      , ("minting-policy", "Generate LenderNFT minting policy", mintingPolicyP)
      ]

    tokenNameP = CompleteCommand <$> tokenNameArg <*> pure LenderNft
    mintingPolicyP = CompleteCommand <$> mintingPolicyArg <*> nftTypeP

    nftTypeP = borrowerNftTypeFlag <|> lenderNftTypeFlag

    borrowerNftTypeFlag = flag' BorrowerNft (
      long "borrower"
      <> short 'b'
      <> help "choose borrower"
      )

    lenderNftTypeFlag = flag' LenderNft (
      long "lender"
      <> short 'l'
      <> help "choose lender"
      )

    tokenNameArg = TokenName <$> strOption
      (mconcat
        [ help "Enter utxo to be consumed when minting LenderNFT."
        , long "utxo"
        , short 'u'
        , metavar "UTXO"
        ])

    mintingPolicyArg = MintingPolicy <$> strOption
      (mconcat
       [ help "Enter name of aada nft minting policy. Default is aada.nft"
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
    CompleteCommand (TokenName utxo) _ -> do
      let (PTX.BuiltinByteString byteString) = unTokenName . getAadaTokenName . parseUTxO $ utxo
      print $ show (encodeHex byteString)
    CompleteCommand (MintingPolicy fp) LenderNft -> do
      writeAadaNftMintingPolicyScript fp True
    CompleteCommand (MintingPolicy fp) BorrowerNft -> do
      writeAadaNftMintingPolicyScript fp False

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