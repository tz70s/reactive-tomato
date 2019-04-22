module Reactive.Tomato.EVar.Tests
  ( tests
  )
where

import Control.Concurrent
import Test.Tasty
import Test.Tasty.HUnit

import Reactive.Tomato

tests :: TestTree
tests =
  testGroup "EVar Tests" [testCase "Use EVar to get event from callback" testGetEventFromCallback]

testGetEventFromCallback :: Assertion
testGetEventFromCallback = do
  evar <- newEVar
  _    <- forkIO $ emit evar (1 :: Int)
  let e0 = events evar
  react e0 $ \num -> num @?= 1
