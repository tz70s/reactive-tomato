module Main
  ( main
  )
where

import           Test.Tasty
import qualified Tomato.Colocation.Tests        as Colocation

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = Colocation.tests
