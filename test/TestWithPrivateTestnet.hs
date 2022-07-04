module Main where

import Prelude
import System.Process

main :: IO ()
main = do
    callCommand "./test/private-testnet/scripts/automate.sh"