{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Main (main) where

import           Prelude
import           Plutus.V1.Ledger.Api

import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.String           as FS
import Data.Text
import Options.Applicative
import Paths_aada_lend (version)
import Data.Version (showVersion)
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

import               Interest
import               Collateral
import               Request
import               Liquidation
import               AadaNft

import Plutus.Model (scriptCurrencySymbol)

import Ply.Core.Serialize
import Ply

import Type.Reflection (Typeable, tyConModule, tyConName, typeRep, typeRepTyCon)

data HashType =
  ValidatorHash' | PubKeyHash'
  deriving Show

data StakingHash' = StakingHash' {
    hash     :: String
  , hashType :: HashType
} deriving Show

data StakingKey =
    StakingKeyHash StakingHash'
  | StakingKeyPtr  Integer Integer Integer
  deriving Show

type ShowVersion = Bool

data Command = Command {
    maybeStakingKey :: Maybe StakingKey
  , interestFp      :: FilePath
  , collateralFp    :: FilePath
  , requestFp       :: FilePath
  , liquidationFp   :: FilePath
  , versionFlag     :: ShowVersion
  , usePlutusV1     :: Bool
} deriving Show

-- ints :: Parser [Integer]
-- ints = many (option auto (short 'p' <> help "StakingKey pointer arguments. Must provide 3 of them"))

plutusV1VersionParser :: Parser Bool
plutusV1VersionParser = switch (help "Compile scripts as Plutus V1 instead of default Plutus V2" <> short 'o' <> long "--old")

versionFlagParser :: Parser ShowVersion
versionFlagParser = switch (help "Show project version" <> long "version")

defaultInterestFp :: FilePath
defaultInterestFp = "interest.plutus"

interestPathParser :: Parser FilePath
interestPathParser = strOption
      (mconcat
       [ help "Enter name of interest validator"
       , long "interest"
       , short 'i'
       , showDefault
       , metavar "FILEPATH"
       , value defaultInterestFp ])

defaultCollateralFp :: FilePath
defaultCollateralFp = "collateral.plutus"

collateralPathParser :: Parser FilePath
collateralPathParser = strOption
      (mconcat
       [ help "Enter name of collateral validator"
       , long "collateral"
       , short 'c'
       , showDefault
       , metavar "FILEPATH"
       , value defaultCollateralFp ])

defaultRequestFp :: FilePath
defaultRequestFp = "request.plutus"

requestPathParser :: Parser FilePath
requestPathParser = strOption
      (mconcat
       [ help "Enter name of request validator"
       , long "request"
       , short 'r'
       , showDefault
       , metavar "FILEPATH"
       , value defaultRequestFp ])

liquidationInterestFp :: FilePath
liquidationInterestFp = "liquidation.plutus"

liquidationPathParser :: Parser FilePath
liquidationPathParser = strOption
      (mconcat
       [ help "Enter name of liquidation validator"
       , long "liquidation"
       , short 'l'
       , showDefault
       , metavar "FILEPATH"
       , value liquidationInterestFp ])

parseFirstPtr :: Parser Integer
parseFirstPtr = option auto (long "first"
  <> metavar "INTEGER"
  <> help "first StakingKey pointer parameter"
  )

parseSecondPtr :: Parser Integer
parseSecondPtr = option auto (long "second"
  <> metavar "INTEGER"
  <> help "second StakingKey pointer parameter"
  )

parseThirdPtr :: Parser Integer
parseThirdPtr = option auto (long "third"
  <> metavar "INTEGER"
  <> help "third StakingKey pointer parameter"
  )

parseStakingKeyPtr :: Parser StakingKey
parseStakingKeyPtr = StakingKeyPtr <$> parseFirstPtr <*> parseSecondPtr <*> parseThirdPtr

parseStakingKeyHashHash :: Parser String
parseStakingKeyHashHash = strOption
          ( long "hash"
         <> metavar "STRING"
         <> help "Staking credential used to assign rewards. The transaction that spends this output must be signed by the key which hash is provided by this parameter" )

parseStakingKeyHash :: Parser StakingHash'
parseStakingKeyHash = StakingHash' <$> parseStakingKeyHashHash <*> parseHashType

parseHashTypeValidator :: Parser HashType
parseHashTypeValidator = flag' ValidatorHash' (help "Interpret provided staking key hash as a Validator Hash" <> short 'v' <> long "validator")

parseHashTypePubKeyHash :: Parser HashType
parseHashTypePubKeyHash = flag' PubKeyHash' (help "Interpret provided staking key hash as PubKeyHash" <> short 'k' <> long "pubkey")

parseHashType :: Parser HashType
parseHashType = parseHashTypePubKeyHash <|> parseHashTypeValidator

parseStakingKey' :: Parser StakingKey
parseStakingKey' = (StakingKeyHash <$> parseStakingKeyHash) <|> parseStakingKeyPtr

parseCommand :: Parser Command
parseCommand = Command <$> optional parseStakingKey' <*>
                           interestPathParser <*>
                           collateralPathParser <*>
                           requestPathParser <*>
                           liquidationPathParser <*>
                           versionFlagParser <*>
                           plutusV1VersionParser

parser :: ParserInfo Command
parser = info (helper <*> parseCommand) fullDesc
-- end of parser

getLenderNftCs :: CurrencySymbol
getLenderNftCs = scriptCurrencySymbol $ AadaNft.aadaNftPolicy True-- getLenderNftPolicy

getBorrowerNftCs :: CurrencySymbol
getBorrowerNftCs = scriptCurrencySymbol $ AadaNft.aadaNftPolicy False -- getBorrowerNftPolicy

getCollateralScParams :: Maybe StakingCredential -> Collateral.ContractInfo
getCollateralScParams stakingCredential = Collateral.ContractInfo {
        Collateral.lenderNftCs    = getLenderNftCs
      , Collateral.borrowersNftCs = getBorrowerNftCs
      , Collateral.interestSc     = Interest.interestAddress (Interest.ContractInfo getLenderNftCs) stakingCredential
    }

getRequestScParams :: Maybe StakingCredential -> Request.ContractInfo
getRequestScParams stakingCredential = Request.ContractInfo {
        Request.lenderNftCs    = getLenderNftCs
      , Request.borrowersNftCs = getBorrowerNftCs
      , Request.collateralSc   = Collateral.collateralAddress (getCollateralScParams stakingCredential) stakingCredential
    }

getStakingCredentialFromOpts :: Command -> Maybe StakingCredential
getStakingCredentialFromOpts opts = case opts of
  Command Nothing _ _ _ _ _ _         -> Nothing
  Command (Just stakeKey) _ _ _ _ _ _ -> case stakeKey of
    StakingKeyHash (StakingHash' h ValidatorHash') -> Just . StakingHash . ScriptCredential . ValidatorHash . getLedgerBytes . FS.fromString $ h
    StakingKeyHash (StakingHash' h PubKeyHash')    -> Just . StakingHash . PubKeyCredential . PubKeyHash . getLedgerBytes . FS.fromString $ h
    StakingKeyPtr a b c                            -> Just $ StakingPtr a b c

main :: IO ()
main = do
  opts <- execParser parser
  case opts of
    (Command _ _ _ _ _ True _) -> print $ showVersion version
    (Command _ _ _ _ _ _ plutusV1) -> do
      let stakeKey = getStakingCredentialFromOpts opts
          scriptVersion = case plutusV1 of
                            True  -> ScriptV1
                            False -> ScriptV2
      writePlutusScript (interestFp opts)    (pack . show $ Interest.ContractInfo getLenderNftCs)      [(typeName @Interest.ContractInfo)]    (Interest.interest (Interest.ContractInfo getLenderNftCs))            scriptVersion
      writePlutusScript (collateralFp opts)  (pack . show $ getCollateralScParams stakeKey)            [(typeName @Collateral.ContractInfo)]  (Collateral.collateralScript (getCollateralScParams stakeKey))        scriptVersion
      writePlutusScript (requestFp opts)     (pack . show $ getRequestScParams stakeKey)               [(typeName @Request.ContractInfo)]     (Request.request (getRequestScParams stakeKey))                       scriptVersion
      writePlutusScript (liquidationFp opts) (pack . show $ Liquidation.ContractInfo getBorrowerNftCs) [(typeName @Liquidation.ContractInfo)] (Liquidation.liquidation $ Liquidation.ContractInfo getBorrowerNftCs) scriptVersion

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
  Script ->
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


writePlutusScript :: FilePath -> Text -> [Typename] -> Script -> ScriptVersion -> IO ()
writePlutusScript filename description typenames script scriptVersion = do
  writeEnvelope' description filename scriptVersion ValidatorRole typenames script
