import Plutus.Test.Model (readDefaultBchConfig)
import Test.Tasty (defaultMain, testGroup)
import Prelude
import Spec.Test

main :: IO ()
main = do
  cfg <- readDefaultBchConfig
  defaultMain $
    testGroup
      "Test Suites"
      [ Spec.Test.tests cfg
      ]