module Main
  ( main
  )
where

import           Test.Tasty
import qualified Reactive.Tomato.Signal.Tests  as Signal

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [Signal.tests]
