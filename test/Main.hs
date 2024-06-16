module Main (main) where

import MachineTest
import System.Exit (exitFailure, exitSuccess)
import Test.HUnit

allTests :: Test
allTests = tests

main :: IO Counts
main = do
  c <- runTestTT allTests
  if errors c == 0 && failures c == 0
    then exitSuccess
    else exitFailure
