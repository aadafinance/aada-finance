{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DerivingStrategies #-}

import           Prelude

import qualified Cardano.Ledger.Alonzo.Data as Alonzo
import qualified Plutus.V1.Ledger.Api       as Plutus
import qualified PlutusTx.Builtins.Internal as PTX

import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.String           as FS
import Text.Hex (encodeHex)
import Data.Text (Text)
import Data.ByteString (ByteString)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import qualified Data.ByteString.Base16 as Base16
import Data.Aeson (object, (.=))
import Data.Aeson.Types (
  FromJSON (parseJSON),
  ToJSON (toJSON),
  Value (Object),
  prependFailure,
  typeMismatch,
  (.:),
 )
import Options.Applicative

import           AadaNft (aadaNftScript)
import Spec.Test

import Ply.Core.Serialize
import Ply

defaultAadaNftFp :: FilePath
defaultAadaNftFp = "aada.nft"

data CompleteCommand = CompleteCommand {
    cmd         :: Command
  , nftType     :: NftType
  } deriving Show

type UsePlutusV1 = Bool

data Command =
    TokenName String
  | MintingPolicy FilePath UsePlutusV1
  deriving Show

data NftType =
    BorrowerNft
  | LenderNft
  deriving Show

plutusV1VersionParser :: Parser UsePlutusV1
plutusV1VersionParser = switch (help "Compile scripts as Plutus V1 instead of default Plutus V2" <> short 'o' <> long "--old")

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

    mintingPolicyArg = MintingPolicy <$> (strOption
      (mconcat
       [ help "Enter name of aada nft minting policy. Default is aada.nft"
       , long "name"
       , short 'p'
       , showDefault
       , metavar "FILEPATH"
       , value defaultAadaNftFp ])) <*> plutusV1VersionParser

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
      let (PTX.BuiltinByteString byteString) = Plutus.unTokenName . getAadaTokenName . parseUTxO $ utxo
      print $ show (encodeHex byteString)
    CompleteCommand (MintingPolicy fp version) LenderNft -> do
      writeAadaNftMintingPolicyScript fp True version
    CompleteCommand (MintingPolicy fp version) BorrowerNft -> do
      writeAadaNftMintingPolicyScript fp False version

parseUTxO :: String -> Plutus.TxOutRef
parseUTxO s =
  let
    (x, y) = span (/= '#') s
  in
    Plutus.TxOutRef (Plutus.TxId $ Plutus.getLedgerBytes $ FS.fromString x) $ read $ tail y

writeAadaNftMintingPolicyScript :: String -> Bool -> UsePlutusV1 -> IO ()
writeAadaNftMintingPolicyScript fp isLenderNft isPlutusV1 = do
  putStrLn $ "Writing output to: " ++ fp
  let description = case isLenderNft of
                      True  -> "LenderToken"
                      False -> "BorrowerToken"
      scriptVersion = case isPlutusV1 of
                       True  -> ScriptV1
                       False -> ScriptV2
  writePlutusMintingScript fp description [(typeName @Bool)] (aadaNftScript isLenderNft) scriptVersion

writePlutusMintingScript :: FilePath -> Text -> [Typename] -> Plutus.Script -> ScriptVersion -> IO ()
writePlutusMintingScript filename description typenames script scriptVersion = do
  writeEnvelope' description filename scriptVersion MintingPolicyRole typenames script

-- | Taken from Ply/Core/Types.hs
-- | JSON schema of the envelope we'll be using to represent typed scripts in the filesystem,
data TypedScriptEnvelope' = TypedScriptEnvelope'
  { -- | Plutus script version.
    tsVersion' :: !ScriptVersion
  , -- | Plutus script role, either a validator or a minting policy.
    tsRole' :: !ScriptRole
  , -- | List of extra parameter types to be applied before being treated as a validator/minting policy.
    tsParamTypes' :: [Typename]
  , -- | Description of the script, not semantically relevant.
    tsDescription' :: !Text
  , -- | The actual script in serialized CBOR form.
    tsCbor' :: !ByteString
  , -- | The actual script in raw serialized form.
    tsRaw' :: !ByteString
  }
  deriving stock (Eq, Show)

instance ToJSON TypedScriptEnvelope' where
  toJSON (TypedScriptEnvelope' ver rol params desc cborHex rawHex) =
    toJSON $
      object
        [ "version" .= ver
        , "role" .= rol
        , "params" .= params
        , "description" .= desc
        , "cborHex" .= Text.decodeUtf8 (Base16.encode cborHex)
        , "rawHex" .= Text.decodeUtf8 (Base16.encode rawHex)
        , "type" .= case ver of
                     ScriptV1 -> "PlutusScriptV1" :: Text
                     ScriptV2 -> "PlutusScriptV2" :: Text
        ]

-- | Write a typed script into a "Ply.Core.Types.TypedScriptEnvelope".
writeEnvelope' ::
  -- | Description for the script (semantically irrelevant).
  Text ->
  -- | Path to write the file to.
  FilePath ->
  -- | Version of the script.
  ScriptVersion ->
  -- | Whether the script is a validator or a minting policy.
  ScriptRole ->
  -- | The extra parameter types for the script.
  [Typename] ->
  -- | The script itself.
  Plutus.Script ->
  IO ()
writeEnvelope' descr filepath scrptVer scrptRole paramTypes scrpt = do
  let plutusEnvelope =
        TypedScriptEnvelope'
          { tsVersion' = scrptVer
          , tsRole' = scrptRole
          , tsParamTypes' = paramTypes
          , tsDescription' = descr
          , tsCbor' = serializeScriptCbor scrpt
          , tsRaw' = serializeScript scrpt
          }
      content = encodePretty plutusEnvelope
  LBS.writeFile filepath content
-- | Taken from Ply/Core/Types.hs