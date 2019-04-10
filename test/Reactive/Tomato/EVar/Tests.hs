module Reactive.Tomato.EVar.Tests
  ( tests
  )
where

import           Test.Tasty
import           Test.Tasty.HUnit
import           Reactive.Tomato
import           Control.Concurrent

tests :: TestTree
tests =
  testGroup "EVar Tests" [testCase "Use EVar to get events from callback" getEventsFromCallback]

getEventsFromCallback :: Assertion
getEventsFromCallback = do
  evar <- newEVar
  _ <- forkIO $ emit (1 :: Int) evar
  let sig1 = events evar
  react sig1 $ \num -> num @?= 1
