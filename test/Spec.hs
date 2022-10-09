import Plutus.Test.Model (readDefaultBchConfig)
import Test.Tasty (defaultMain, testGroup)
import Prelude
import Spec.Test
import Spec.Liquidator.Test

main :: IO ()
main = do
  cfg <- readDefaultBchConfig
  defaultMain $
    testGroup
      "Test Suites"
      [
        Spec.Test.mainTests cfg
        -- Spec.Liquidator.Test.liquidatorTests cfg
      -- , Spec.Test.testSize cfg
      -- , Spec.Test.mintOracleNftTests cfg
      ]