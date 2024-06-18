module Main (main) where

import qualified MachineTest
import qualified OnigurumaTest
import System.Exit (exitFailure, exitSuccess)
import Test.HUnit

allTests :: Test
allTests =
  test
    [ MachineTest.tests,
      OnigurumaTest.tests
    ]

main :: IO Counts
main = do
  c <- runTestTT allTests
  if errors c == 0 && failures c == 0
    then exitSuccess
    else exitFailure
