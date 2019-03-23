module Main
  ( main
  )
where

import           Test.Tasty
import qualified Tomato.Colocation.Test        as Colocation

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = Colocation.tests
