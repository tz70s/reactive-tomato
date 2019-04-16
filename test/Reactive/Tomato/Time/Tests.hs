module Reactive.Tomato.Time.Tests
  ( tests
  )
where

import           Test.Tasty
import           Test.Tasty.HUnit
import           Reactive.Tomato               as RT
import           System.IO
import           Control.Monad.IO.Class
import           Control.Applicative

tests :: TestTree
tests = testGroup
  "Time Tests"
  [ testCase "Throttling effect" testThrottle
  , testCase "Snapshot effect"   testSnapshot
  , testCase "Window effect"     testWindow
  ]

testThrottle :: Assertion
testThrottle = do
  timer <- every $ second 1
  let sig0 = throttle timer $ listGen ([1, 2, 3, 4, 5] :: [Int])
  let sig1 = sig0 >>= printx
  xs <- interpretM sig1
  xs @?= [1, 2, 3, 4, 5]
 where
  printx x = do
    liftIO $ print x
    return x

testSnapshot :: Assertion
testSnapshot = do
  hSetBuffering stdout LineBuffering
  timer0 <- every $ milli 80
  timer1 <- every $ second 1
  let sig0    = throttle timer0 $ listGen ([1 .. 10] :: [Int])
  let snap    = snapshot timer1 sig0
  let testsig = liftA2 const snap $ listGen ([1, 2, 3] :: [Int])
  xs <- interpretM testsig
  -- Mostly, this will print [1, 10, 10]
  -- However, this is non-determinism.
  print xs

testWindow :: Assertion
testWindow = do
  hSetBuffering stdout LineBuffering
  timer0 <- every $ milli 100
  timer1 <- every $ second 1
  let sig0    = throttle timer0 $ listGen ([1 .. 10] :: [Int])
  -- sig1 :: Signal IO (IO Int)
  let sig1    = RT.last <$> window timer1 sig0
  let testsig = liftA2 const sig1 $ listGen ([1, 2] :: [Int])
  react testsig (>>= print)
