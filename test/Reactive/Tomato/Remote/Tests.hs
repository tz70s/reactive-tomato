{-# LANGUAGE OverloadedStrings #-}

module Reactive.Tomato.Remote.Tests
  ( tests
  )
where

import Test.Tasty
import Test.Tasty.HUnit

import Reactive.Tomato as RT
import Reactive.Tomato.Time
import Reactive.Tomato.Remote

tests :: TestTree
tests = testGroup
  "Remote Tests"
  [ testCase "Cluster monad initialization" testClusterMonadInit
  , testCase "Stateful counter"             testStatefulConter
  ]

testClusterMonadInit :: Assertion
testClusterMonadInit = do
  num <- runCluster defaultLocal $ return (5 :: Int)
  num @?= 5

testStatefulConter :: Assertion
testStatefulConter = do
  let cnt = foldp (+) 0 (RT.repeat (1 :: Int))
  updateTimer <- every $ milli 10
  let updates = throttle updateTimer cnt
  cnt1 <- runCluster defaultLocal $ do
    -- This is useful to eliminate explicit sid construction.
    -- If there's a sid which will be reuse,
    -- calling 'remote' will inference the sid phantom type as well as the signal type.
    sid0 <- sid "cnt0"
    -- Both spawn and remote will fork new threads for asynchronously driving event propagation.
    -- You can explicitly cancel sid by cancelSid, however, it'll atomatically if events terminate.
    _    <- spawn sid0 updates
    filterJust <$> remote sid0
  xs <- interpret (RT.take 10 cnt1)
  xs @?= [1 .. 10]
