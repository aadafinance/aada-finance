import Test.Tasty (defaultMain, testGroup)
import Prelude
import Spec.Test
import Plutus.Model

main :: IO ()
main = do
  let cfg = defaultBabbage
  defaultMain $
    testGroup
      "Test Suites"
      [ Spec.Test.mainTests cfg
      -- , Spec.Test.testSize cfg
      -- , Spec.Test.mintOracleNftTests cfg
      ]