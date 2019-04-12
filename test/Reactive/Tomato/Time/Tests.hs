module Reactive.Tomato.Time.Tests
  ( tests
  )
where

import           Test.Tasty
import           Test.Tasty.HUnit
import           Reactive.Tomato

tests :: TestTree
tests = testGroup "Time Tests" [testCase "Throttling effect" testThrottle]

testThrottle :: Assertion
testThrottle = do
  timer <- every 1000
  let sig0 = throttle timer $ listGen ([1, 2, 3, 4, 5] :: [Int])
  xs <- interpretM sig0
  take 5 xs @?= [1, 2, 3, 4, 5]
