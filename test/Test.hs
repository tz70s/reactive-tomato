module Main
  ( main
  )
where

import           Test.Tasty
import           Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [testCase "no test given" $ 1 + 1 @?= 2]
