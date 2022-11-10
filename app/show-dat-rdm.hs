{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Aeson.Encode.Pretty as Json
import Data.ByteString.Lazy qualified as LB
import Prelude

import Cardano.Api
    ( scriptDataToJson,
      ScriptDataJsonSchema(ScriptDataJsonDetailedSchema) )
import Cardano.Api.Shelley ( fromPlutusData )
import qualified PlutusTx
import Ledger
import Plutus.V1.Ledger.Credential
import Plutus.V1.Ledger.Value
import Spec.Test
import Liquidator.SafetyModule as Sm

main :: IO ()
main = do
  let exampleOracleRedeemer = Redeemer (PlutusTx.toBuiltinData (0 :: Integer))
  writeData "redeemer-of-oracleNft.json" exampleOracleRedeemer
  let exampleDatum = getTestDatum 0 "ff" "ff" "ff" 0 "nfttn" 0 (Just . StakingHash . PubKeyCredential . PubKeyHash $ "ff")
  writeData "example.datum" exampleDatum
  writeData "example-collateral-redeemer.json" (POSIXTime 2)
  let exampleRequestRedeemer = Redeemer (PlutusTx.toBuiltinData $ getAadaTokenName (TxOutRef "ff" 1))
  writeData "example-request-redeemer.json" exampleRequestRedeemer
  let exampleAadaNftRedeemer = Redeemer (PlutusTx.toBuiltinData (TxOutRef "ff" 1))
  writeData "example-aada-nft-redeemer.json" exampleAadaNftRedeemer
  let liquidationActionCancel = Redeemer (PlutusTx.toBuiltinData Sm.Cancel)
  writeData "example-liquidation-action-cancel.json" liquidationActionCancel
  let liquidationActionCancel = Redeemer (PlutusTx.toBuiltinData Sm.Cancel)
  writeData "example-liquidation-action-cancel.json" liquidationActionCancel
  let liquidationActionLiquidateByDeadline = Redeemer (PlutusTx.toBuiltinData Sm.LiquidateByDeadline)
  writeData "example-liquidation-action-liquidate-by-deadline.json" liquidationActionLiquidateByDeadline
  let liquidationActionLiquidateWithOracle = Redeemer (PlutusTx.toBuiltinData Sm.LiquidateWithOracle)
  writeData "example-liquidation-action-liquidate-with-oracle.json" liquidationActionLiquidateWithOracle
  let liquidateDatum = Datum (PlutusTx.toBuiltinData $ unTokenName "ff")
  writeData "example-liquidation-datum.json" liquidateDatum
  putStrLn "Done"

writeData :: PlutusTx.ToData a => FilePath -> a -> IO ()
writeData file isData = do
  print file
  LB.writeFile file (toJsonString isData)

toJsonString :: PlutusTx.ToData a => a -> LB.ByteString
toJsonString =
  Json.encodePretty
    . scriptDataToJson ScriptDataJsonDetailedSchema
    . fromPlutusData
    . PlutusTx.toData
