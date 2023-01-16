{-# LANGUAGE OverloadedStrings  #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Main (main) where

import           Prelude
import           Cardano.Api hiding (Address)
import           Cardano.Api.Shelley hiding (Address)

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
import               DebtRequest
import               Liquidation
import               AadaNft
import               Liquidator.SafetyModule as Safety
import               Liquidator.SafetyToken as Safety

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
  , debtRequestFp   :: FilePath
  , liquidationFp   :: FilePath
  , safetyModulefp  :: FilePath
  , versionFlag     :: ShowVersion
  , minPercentage   :: Integer
} deriving Show

-- ints :: Parser [Integer]
-- ints = many (option auto (short 'p' <> help "StakingKey pointer arguments. Must provide 3 of them"))

versionFlagParser :: Parser ShowVersion
versionFlagParser = switch (help "Show project version" <> long "version")

defaultSafetyModuleFp :: FilePath
defaultSafetyModuleFp = "safety_module.plutus"

safetyModuleFpParser :: Parser FilePath
safetyModuleFpParser = strOption
      (mconcat
       [ help "Enter name of safety module validator"
       , long "safety"
       , short 's'
       , showDefault
       , metavar "FILEPATH"
       , value defaultSafetyModuleFp ])

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

defaultDebtRequestFp :: FilePath
defaultDebtRequestFp = "debt_request.plutus"

debtRequestPathParser :: Parser FilePath
debtRequestPathParser = strOption
      (mconcat
       [ help "Enter name of debt request validator"
       , long "debt"
       , short 'd'
       , showDefault
       , metavar "FILEPATH"
       , value defaultDebtRequestFp ])

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

defaultPercentage :: Integer
defaultPercentage = 200000

parseMinimumPercentage :: Parser Integer
parseMinimumPercentage = option auto (long "fee"
  <> metavar "INTEGER"
  <> help "Enter minimum interest fee percentage. Resulting percentage is number provided divded by 1_000_000"
  <> showDefault
  <> value defaultPercentage
  )

parseCommand :: Parser Command
parseCommand = Command <$> optional parseStakingKey'
  <*> interestPathParser
  <*> collateralPathParser
  <*> requestPathParser
  <*> debtRequestPathParser
  <*> liquidationPathParser
  <*> safetyModuleFpParser
  <*> versionFlagParser
  <*> parseMinimumPercentage

parser :: ParserInfo Command
parser = info (helper <*> parseCommand) fullDesc

getSafetyTokenCs :: CurrencySymbol
getSafetyTokenCs = scriptCurrencySymbol $ Safety.policy getLenderNftCs

getLenderNftPolicy :: MintingPolicy
getLenderNftPolicy = AadaNft.policy True

getBorrowerNftPolicy :: MintingPolicy
getBorrowerNftPolicy = AadaNft.policy False

getLenderNftCs :: CurrencySymbol
getLenderNftCs = scriptCurrencySymbol getLenderNftPolicy

getBorrowerNftCs :: CurrencySymbol
getBorrowerNftCs = scriptCurrencySymbol getBorrowerNftPolicy

getCollateralScParams :: Maybe StakingCredential -> Integer -> Collateral.ContractInfo
getCollateralScParams stakingCredential minPercentage = Collateral.ContractInfo {
        Collateral.lenderNftCs              = getLenderNftCs
      , Collateral.borrowersNftCs           = getBorrowerNftCs
      , Collateral.interestSc               = Address (ScriptCredential (validatorHash (Interest.validator (Interest.ContractInfo getLenderNftCs)))) stakingCredential
      , Collateral.minInterestFeePercentage = minPercentage
    }

getRequestScParams :: Maybe StakingCredential -> Integer -> Request.ContractInfo
getRequestScParams stakingCredential minPercentage = Request.ContractInfo {
        Request.lenderNftCs    = getLenderNftCs
      , Request.borrowersNftCs = getBorrowerNftCs
      , Request.collateralSc   = Address (ScriptCredential (validatorHash $ Collateral.validator (getCollateralScParams stakingCredential minPercentage))) stakingCredential
    }

getDebtRequestScParams :: Maybe StakingCredential -> Integer -> DebtRequest.ContractInfo
getDebtRequestScParams stakingCredential minPercentage = DebtRequest.ContractInfo {
        DebtRequest.lenderNftCs    = getLenderNftCs
      , DebtRequest.borrowersNftCs = getBorrowerNftCs
      , DebtRequest.collateralSc   = Address (ScriptCredential (validatorHash $ Collateral.validator (getCollateralScParams stakingCredential minPercentage))) stakingCredential
    }

getStakingCredentialFromOpts :: Command -> Maybe StakingCredential
getStakingCredentialFromOpts opts = case opts of
  Command Nothing _ _ _ _ _ _ _ _       -> Nothing
  Command (Just stakeKey) _ _ _ _ _ _ _ _ -> case stakeKey of
    StakingKeyHash (StakingHash' h ValidatorHash') -> Just . StakingHash . ScriptCredential . ValidatorHash . getLedgerBytes . FS.fromString $ h
    StakingKeyHash (StakingHash' h PubKeyHash') -> Just . StakingHash . PubKeyCredential . PubKeyHash . getLedgerBytes . FS.fromString $ h
    StakingKeyPtr a b c -> Just $ StakingPtr a b c

getCollateralAddr :: Maybe StakingCredential -> Integer -> Address
getCollateralAddr stakingCredential minPercentage = Collateral.collateralAddress $ getCollateralScParams stakingCredential minPercentage

getInterestLiquidateAddr :: Address
getInterestLiquidateAddr = Interest.interestAddress $ Interest.ContractInfo getSafetyTokenCs

getSafetyModuleParams :: Maybe StakingCredential -> Integer -> Safety.ContractInfo
getSafetyModuleParams stakingCredential minPercentage = Safety.ContractInfo getLenderNftCs (getCollateralAddr stakingCredential minPercentage) getInterestLiquidateAddr getSafetyTokenCs

getMinFeePercentageFromOpts :: Command -> Integer
getMinFeePercentageFromOpts opts = case opts of
  Command Nothing _ _ _ _ _ _ _ amount -> amount
  _ -> defaultPercentage

main :: IO ()
main = do
  opts <- execParser parser
  case opts of
    (Command _ _ _ _ _ _ _ True _) -> print $ showVersion version
    _ -> do
      let stakeKey = getStakingCredentialFromOpts opts
          scriptnum = 0
          minFee = getMinFeePercentageFromOpts opts
      writePlutusScript scriptnum (interestFp opts) (Interest.interestScript (Interest.ContractInfo getLenderNftCs)) (interestShortBs (Interest.ContractInfo getLenderNftCs))
      writePlutusScript scriptnum (collateralFp opts) (Collateral.collateralScript (getCollateralScParams stakeKey minFee)) (collateralShortBs (getCollateralScParams stakeKey minFee))
      writePlutusScript scriptnum (requestFp opts) (Request.request (getRequestScParams stakeKey minFee)) (requestShortBs (getRequestScParams stakeKey minFee))
      writePlutusScript scriptnum (debtRequestFp opts) (DebtRequest.debtRequest (getDebtRequestScParams stakeKey minFee)) (debtRequestShortBs (getDebtRequestScParams stakeKey minFee))
      writePlutusScript scriptnum (liquidationFp opts) (Liquidation.liquidation $ Liquidation.ContractInfo getBorrowerNftCs) (liquidationShortBs  $ Liquidation.ContractInfo getBorrowerNftCs)
      writePlutusScript scriptnum (safetyModulefp opts) (Safety.safetyScript $ getSafetyModuleParams stakeKey minFee) (liquidationShortBs  $ Liquidation.ContractInfo getBorrowerNftCs)

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
