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
import Ledger

import OracleNft
import Collateral
import Spec.Test

main :: IO ()
main = do
  let exampleOracleRedeemer = Redeemer (PlutusTx.toBuiltinData (0 :: Integer))
  writeData "redeemer-of-oracleNft.json" exampleOracleRedeemer
  let exampleDatum = getTestDatum 0 "ff" "ff" (PaymentPubKeyHash "ff") 0
  writeData "example.datum" exampleDatum
  let exampleCollateralRedeemer = CollateralRedeemer 1 2
  writeData "example-collateral-redeemer.json" exampleCollateralRedeemer
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
