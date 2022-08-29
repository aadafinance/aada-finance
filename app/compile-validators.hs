{-# LANGUAGE OverloadedStrings  #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Main (main) where

import           Prelude
import           Cardano.Api
import           Cardano.Api.Shelley

import qualified Cardano.Ledger.Alonzo.Data as Alonzo
import           Plutus.V1.Ledger.Api
import           Ledger                 (validatorHash, scriptCurrencySymbol)

import qualified Data.ByteString.Short as SBS
import qualified Data.String           as FS
import Options.Applicative
import Paths_aada_lend (version)
import Data.Version (showVersion)

import               Interest
import               Collateral
import               Request
import               Liquidation
import               AadaNft

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
} deriving Show

-- ints :: Parser [Integer]
-- ints = many (option auto (short 'p' <> help "StakingKey pointer arguments. Must provide 3 of them"))

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
parseCommand = Command <$> optional parseStakingKey' <*> interestPathParser <*>  collateralPathParser <*> requestPathParser <*> liquidationPathParser <*> versionFlagParser

parser :: ParserInfo Command
parser = info (helper <*> parseCommand) fullDesc

getLenderNftPolicy :: MintingPolicy
getLenderNftPolicy = AadaNft.policy True

getBorrowerNftPolicy :: MintingPolicy
getBorrowerNftPolicy = AadaNft.policy False

getLenderNftCs :: CurrencySymbol
getLenderNftCs = scriptCurrencySymbol getLenderNftPolicy

getBorrowerNftCs :: CurrencySymbol
getBorrowerNftCs = scriptCurrencySymbol getBorrowerNftPolicy

getCollateralScParams :: Maybe StakingCredential -> Collateral.ContractInfo
getCollateralScParams stakingCredential = Collateral.ContractInfo {
        Collateral.lenderNftCs    = getLenderNftCs
      , Collateral.borrowersNftCs = getBorrowerNftCs
      , Collateral.interestSc     = Address (ScriptCredential (validatorHash (Interest.validator (Interest.ContractInfo getLenderNftCs)))) stakingCredential
    }

getRequestScParams :: Maybe StakingCredential -> Request.ContractInfo
getRequestScParams stakingCredential = Request.ContractInfo {
        Request.lenderNftCs    = getLenderNftCs
      , Request.borrowersNftCs = getBorrowerNftCs
      , Request.collateralSc   = Address (ScriptCredential (validatorHash $ Collateral.validator (getCollateralScParams stakingCredential))) stakingCredential
    }

getStakingCredentialFromOpts :: Command -> Maybe StakingCredential
getStakingCredentialFromOpts opts = case opts of
  Command Nothing _ _ _ _ _         -> Nothing
  Command (Just stakeKey) _ _ _ _ _ -> case stakeKey of
    StakingKeyHash (StakingHash' h ValidatorHash') -> Just . StakingHash . ScriptCredential . ValidatorHash . getLedgerBytes . FS.fromString $ h
    StakingKeyHash (StakingHash' h PubKeyHash') -> Just . StakingHash . PubKeyCredential . PubKeyHash . getLedgerBytes . FS.fromString $ h
    StakingKeyPtr a b c -> Just $ StakingPtr a b c

main :: IO ()
main = do
  opts <- execParser parser
  case opts of
    (Command _ _ _ _ _ f) -> print $ showVersion version
    _ -> do
      let stakeKey = getStakingCredentialFromOpts opts
          scriptnum = 0
      writePlutusScript scriptnum (interestFp opts) (Interest.interestScript (Interest.ContractInfo getLenderNftCs)) (interestShortBs (Interest.ContractInfo getLenderNftCs))
      writePlutusScript scriptnum (collateralFp opts) (Collateral.collateralScript (getCollateralScParams stakeKey)) (collateralShortBs (getCollateralScParams stakeKey))
      writePlutusScript scriptnum (requestFp opts) (Request.request (getRequestScParams stakeKey)) (requestShortBs (getRequestScParams stakeKey))
      writePlutusScript scriptnum (liquidationFp opts) (Liquidation.liquidation $ Liquidation.ContractInfo getBorrowerNftCs) (liquidationShortBs  $ Liquidation.ContractInfo getBorrowerNftCs)

writePlutusScript :: Integer -> FilePath -> PlutusScript PlutusScriptV1 -> SBS.ShortByteString -> IO ()
writePlutusScript scriptnum filename scriptSerial scriptSBS =
  do
  case defaultCostModelParams of
        Just m ->
          let Alonzo.Data pData = toAlonzoData (ScriptDataNumber scriptnum)
              (logout, e) = evaluateScriptCounting Verbose m scriptSBS [pData]
          in do print ("Log output" :: String) >> print logout
                case e of
                  Left evalErr -> print ("Eval Error" :: String) >> print evalErr
                  Right exbudget -> print ("Ex Budget" :: String) >> print exbudget

        Nothing -> error "defaultCostModelParams failed"
  result <- writeFileTextEnvelope filename Nothing scriptSerial
  case result of
    Left err -> print $ displayError err
    Right () -> return ()