{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Aeson.Encode.Pretty as Json
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16 (encode)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Aeson
import qualified Data.Vector as Vector

import Prelude

import qualified PlutusTx
import Plutus.V1.Ledger.Credential
import Plutus.V1.Ledger.Scripts
import Plutus.V1.Ledger.Crypto
import Plutus.V1.Ledger.Time
import Plutus.V2.Ledger.Contexts
import qualified Plutus.V2.Ledger.Api as Plutus
import Spec.Test

-- this code is taken from https://github.com/input-output-hk/cardano-node/tree/master/cardano-api
data ScriptData = ScriptDataConstructor
                                        Integer                     -- ^ Tag for the constructor
                                        [ScriptData]                -- ^ Constructor arguments
                | ScriptDataMap         [(ScriptData, ScriptData)]  -- ^ Key value pairs
                | ScriptDataList        [ScriptData]                -- ^ Elements
                | ScriptDataNumber      Integer
                | ScriptDataBytes       BS.ByteString
  deriving (Eq, Ord, Show)

scriptDataToJsonDetailedSchema :: ScriptData -> Aeson.Value
scriptDataToJsonDetailedSchema = conv
  where
    conv :: ScriptData -> Aeson.Value
    conv (ScriptDataNumber n) = singleFieldObject "int"
                              . Aeson.Number
                              $ fromInteger n
    conv (ScriptDataBytes bs) = singleFieldObject "bytes"
                              . Aeson.String
                              $ Text.decodeLatin1 (Base16.encode bs)
    conv (ScriptDataList  vs) = singleFieldObject "list"
                              . Aeson.Array
                              $ Vector.fromList (map conv vs)
    conv (ScriptDataMap  kvs) = singleFieldObject "map"
                              . Aeson.Array
                              $ Vector.fromList
                                  [ Aeson.object [ ("k", conv k), ("v", conv v) ]
                                  | (k, v) <- kvs ]

    conv (ScriptDataConstructor n vs) =
      Aeson.object
        [ ("constructor", Aeson.Number (fromInteger n))
        , ("fields",      Aeson.Array (Vector.fromList (map conv vs)))
        ]

    singleFieldObject name v = Aeson.object [(name, v)]

fromPlutusData :: Plutus.Data -> ScriptData
fromPlutusData (Plutus.Constr int xs)
                                = ScriptDataConstructor int
                                    [ fromPlutusData x | x <- xs ]
fromPlutusData (Plutus.Map kvs) = ScriptDataMap
                                    [ (fromPlutusData k, fromPlutusData v)
                                    | (k,v) <- kvs ]
fromPlutusData (Plutus.List xs) = ScriptDataList
                                    [ fromPlutusData x | x <- xs ]
fromPlutusData (Plutus.I     n) = ScriptDataNumber n
fromPlutusData (Plutus.B    bs) = ScriptDataBytes bs
-- this code is taken from https://github.com/input-output-hk/cardano-node/tree/master/cardano-api

main :: IO ()
main = do
  let exampleOracleRedeemer = Redeemer (PlutusTx.toBuiltinData (0 :: Integer))
  writeData "redeemer-of-oracleNft.json" exampleOracleRedeemer
  let exampleDatum = getTestDatum
                       0                                                           -- RepayInterval
                       "ff"                                                        -- BorrowerTokenName
                       "ff"                                                        -- LiquidationNftCs
                       "ff"                                                        -- BorrowersAddressPkh
                       0                                                           -- RequestExpirationDate
                       "nfttn"                                                     -- LenderTokenName
                       0                                                           -- LendDate
                       (Just . StakingHash . PubKeyCredential . PubKeyHash $ "ff") -- Maybe StakingCredential
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
  Json.encodePretty
    . scriptDataToJsonDetailedSchema
    . fromPlutusData
    . PlutusTx.toData
