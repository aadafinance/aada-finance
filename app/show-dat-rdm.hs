{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson as Json ( encode )
import Data.ByteString.Lazy qualified as LB
import System.Environment ( getArgs )
import Prelude
import Data.String (fromString)

import Cardano.Api
    ( scriptDataToJson,
      ScriptDataJsonSchema(ScriptDataJsonDetailedSchema) )
import Cardano.Api.Shelley ( fromPlutusData )
import qualified PlutusTx

import OracleNft

main :: IO ()
main = do
  let exampleOracleRedeemer = OracleData (AssetData "ff" "Loan" 1_000_000) (AssetData "ff" "Collateral" 1_000_000) (AssetData "ff" "Interest" 1_000_000) 5
  writeData "redeemer-of-oracleNft.json" exampleOracleRedeemer
  putStrLn "Done"

writeData :: PlutusTx.ToData a => FilePath -> a -> IO ()
writeData file isData = do
  print file
  LB.writeFile file (toJsonString isData)

toJsonString :: PlutusTx.ToData a => a -> LB.ByteString
toJsonString =
  Json.encode
    . scriptDataToJson ScriptDataJsonDetailedSchema
    . fromPlutusData
    . PlutusTx.toData