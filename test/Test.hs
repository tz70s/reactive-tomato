module Main
  ( main
  )
where

import           Test.Tasty
import qualified Reactive.Tomato.Signal.Test   as Signal

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = Signal.tests
