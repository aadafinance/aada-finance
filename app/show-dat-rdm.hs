{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson as Json ( encode )
import Data.ByteString.Lazy qualified as LB
import Prelude

import Cardano.Api
    ( scriptDataToJson,
      ScriptDataJsonSchema(ScriptDataJsonDetailedSchema) )
import Cardano.Api.Shelley ( fromPlutusData )
import qualified PlutusTx
import Plutus.V1.Ledger.Value
import Ledger
import Spec.Test

main :: IO ()
main = do
  let exampleOracleRedeemer = Redeemer (PlutusTx.toBuiltinData (0 :: Integer))
  writeData "redeemer-of-oracleNft.json" exampleOracleRedeemer
  let exampleDatum = getTestDatum 0 "ff" "ff" "ff" 0 "nfttn" 0
  writeData "example.datum" exampleDatum
  writeData "example-collateral-redeemer.json" (POSIXTime 2)
  let exampleRequestRedeemer = Redeemer (PlutusTx.toBuiltinData $ getAadaTokenName (TxOutRef "ff" 1))
  writeData "example-request-redeemer.json" exampleRequestRedeemer
  let exampleAadaNftRedeemer = Redeemer (PlutusTx.toBuiltinData (TxOutRef "ff" 1))
  writeData "example-aada-nft-redeemer.json" exampleAadaNftRedeemer
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
