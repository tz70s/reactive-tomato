module Reactive.Tomato.Time.Tests
  ( tests
  )
where

import Control.Applicative
import System.IO
import Test.Tasty
import Test.Tasty.HUnit

import Reactive.Tomato as RT
import Reactive.Tomato.Time

tests :: TestTree
tests = testGroup
  "Time Tests"
  [testCase "Throttling effect" testThrottle, testCase "Snapshot effect" testSnapshot]

testThrottle :: Assertion
testThrottle = do
  timer <- every $ milli 10
  let e0 = throttle timer $ generate ([1, 2, 3, 4, 5] :: [Int])
  react e0 print

testSnapshot :: Assertion
testSnapshot = do
  hSetBuffering stdout LineBuffering
  timer0 <- every $ milli 10
  timer1 <- every $ milli 100
  let e0      = throttle timer0 $ generate ([1 .. 10] :: [Int])
  let snap    = snapshot timer1 e0
  let testsig = liftA2 const snap $ generate ([1, 2, 3] :: [Int])
  xs <- interpret testsig
  -- Mostly, this will print [1, 10, 10]
  -- However, this is non-determinism.
  print xs
