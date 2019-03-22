module Main
  ( main
  )
where

import           Test.Tasty
import qualified Tomato.ColocationTest         as Colocation

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = Colocation.tests
