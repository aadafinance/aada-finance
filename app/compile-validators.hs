{-# LANGUAGE OverloadedStrings  #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Main (main) where

import           Prelude
import           System.Environment
import           Cardano.Api
import           Cardano.Api.Shelley

import qualified Cardano.Ledger.Alonzo.Data as Alonzo
import           Plutus.V1.Ledger.Api
import           Ledger                 (validatorHash, scriptCurrencySymbol)

import qualified Data.ByteString.Short as SBS
import qualified Data.String           as FS
import Options.Applicative

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

data Command = Command {
  maybeStakingKey :: Maybe StakingKey
} deriving Show

-- ints :: Parser [Integer]
-- ints = many (option auto (short 'p' <> help "StakingKey pointer arguments. Must provide 3 of them"))

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
         <> help "Credential required to unlock a transaction output")

parseStakingKeyHash :: Parser StakingHash'
parseStakingKeyHash = StakingHash' <$> parseStakingKeyHashHash <*> parseHashType

parseHashTypeValidator :: Parser HashType
parseHashTypeValidator = flag' ValidatorHash' (help "Validator Hash" <> short 'v' <> long "validator")

parseHashTypePubKeyHash :: Parser HashType
parseHashTypePubKeyHash = flag' PubKeyHash' (help "PubKeyHash" <> short 'k' <> long "pubkey")

parseHashType :: Parser HashType
parseHashType = parseHashTypePubKeyHash <|> parseHashTypeValidator

parseStakingKey' :: Parser StakingKey
parseStakingKey' = (StakingKeyHash <$> parseStakingKeyHash) <|> parseStakingKeyPtr

parseCommand :: Parser Command
parseCommand = Command <$> optional parseStakingKey'

parser :: ParserInfo Command
parser = info parseCommand fullDesc

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
  Command Nothing -> Nothing
  Command (Just stakeKey) -> case stakeKey of
    StakingKeyHash (StakingHash' h ValidatorHash') -> Just . StakingHash . ScriptCredential . ValidatorHash . getLedgerBytes . FS.fromString $ h
    StakingKeyHash (StakingHash' h PubKeyHash') -> Just . StakingHash . PubKeyCredential . PubKeyHash . getLedgerBytes . FS.fromString $ h
    StakingKeyPtr a b c -> Just $ StakingPtr a b c

main :: IO ()
main = do
  opts <- execParser parser
  let stakeKey = getStakingCredentialFromOpts opts
      scriptnum = 0
  writePlutusScript scriptnum "interest.plutus" (Interest.interestScript (Interest.ContractInfo getLenderNftCs)) (interestShortBs (Interest.ContractInfo getLenderNftCs))
  writePlutusScript scriptnum "collateral.plutus" (Collateral.collateralScript (getCollateralScParams stakeKey)) (collateralShortBs (getCollateralScParams stakeKey))
  writePlutusScript scriptnum "request.plutus" (Request.request (getRequestScParams stakeKey)) (requestShortBs (getRequestScParams stakeKey))
  writePlutusScript scriptnum "liquidation.plutus" Liquidation.liquidation liquidationShortBs

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