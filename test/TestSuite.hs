module Main
  ( main
  )
where

import Test.Tasty

import qualified Reactive.Tomato.EVar.Tests as EVar
import qualified Reactive.Tomato.Remote.Tests as Remote
import qualified Reactive.Tomato.Event.Tests as Event
import qualified Reactive.Tomato.Time.Tests as Time

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [Event.tests, EVar.tests, Time.tests, Remote.tests]
