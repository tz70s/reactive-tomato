module Reactive.Tomato.Signal.Tests
  ( tests
  )
where

import Control.Concurrent.Async
import Control.Applicative
import Test.Tasty
import Test.Tasty.HUnit

import Reactive.Tomato as RT
import Reactive.Tomato.Time

tests :: TestTree
tests = testGroup
  "Signal Tests"
  [ testCase "Changes to propagate events"              testChanges
  , testCase "Signal can be consumed by multiple event" testBroadcastEff
  ]

testChanges :: Assertion
testChanges = do
  let e0 = generate [1 .. 10]
  c0     <- signal 1 e0
  timer0 <- every $ milli 10
  let e1 = throttle timer0 $ sample c0 (RT.repeat id)
  xs <- interpret $ RT.take 10 e1
  xs @?= (replicate 10 10 :: [Int])

testBroadcastEff :: Assertion
testBroadcastEff = do
  evar <- newEVar
  forConcurrently_ ([1 .. 5] :: [Int]) $ \num -> emit evar num
  let e0 = events evar
  c0     <- signal 1 e0
  timer0 <- every $ milli 10
  let e1 = throttle timer0 $ (+ 1) <$> sample c0 (RT.repeat id)
  let e2 = throttle timer0 $ sample c0 (RT.repeat id)
  let e3 = liftA2 (,) e1 e2
  react (RT.take 10 e3) print
