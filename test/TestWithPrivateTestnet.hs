module Main where

import Prelude
import System.Process
import System.IO
import Data.List

waitForFinish :: Handle -> IO ()
waitForFinish h = do
  putStrLn "waiting for sync to finish"
  ios <- hGetLine h
  putStrLn ios
  if "Congrats! Your network is ready for use!" `isInfixOf` ios
    then pure ()
    else waitForFinish h

main :: IO ()
main = do
    (_,mb_stdo_h, _, p) <- createProcess (shell "test/RunScripts.sh") {std_out = CreatePipe}
    _ <- case mb_stdo_h of
        Just stdo_h -> waitForFinish stdo_h
        Nothing     -> pure ()
    putStrLn "THE END"