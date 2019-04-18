{-# LANGUAGE OverloadedStrings #-}

module Reactive.Tomato.Remote.Tests
  ( tests
  )
where

import           Test.Tasty
import           Test.Tasty.HUnit

import           Reactive.Tomato               as RT

tests :: TestTree
tests = testGroup
  "Remote Tests"
  [ testCase "Cluster monad initialization" testClusterMonadInit
  , testCase "Stateful counter"             testStatefulConter
  ]

testClusterMonadInit :: Assertion
testClusterMonadInit = do
  num <- runCluster defaultLocalPubSub $ return (5 :: Int)
  num @?= 5

testStatefulConter :: Assertion
testStatefulConter = do
  let cnt = foldp (+) 0 $ constant (1 :: Int)
  updateTimer <- every $ milli 10
  let updates = throttle updateTimer cnt
  cnt1 <- runCluster defaultLocalPubSub $ do
    -- This is useful to eliminate explicit sid construction.
    -- If there's a sid which will be reuse,
    -- calling 'remote' will inference the sid phantom type as well as the signal type.
    sid0 <- sid "cnt0"
    -- In general, spawn will block the thread until the signal is terminated.
    -- Therefore, the Cluster monad is instance of MonadFork that you can fork the spawn into separate threads.
    _    <- fork $ spawn sid0 updates
    remote sid0
  xs <- interpretM (RT.take 10 cnt1)
  xs @?= [1 .. 10]
