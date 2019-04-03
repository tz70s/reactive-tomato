module Reactive.Tomato.Signal.Tests
  ( tests
  )
where

import           Test.Tasty
import           Test.Tasty.HUnit
import           Control.Monad.Identity
import           Reactive.Tomato                ( Signal
                                                , interpret
                                                , constant
                                                )

tests :: TestTree
tests = testGroup
  "Signal Tests"
  [ testCase "Constant constructor" constSignal
  , testCase "Functor instance"     functorSignal
  , testCase "Applicative instance" applicativeSignal
  ]

-- TODO: we should encapuslate the unSignal function.
sample :: Signal Identity a -> Int -> [a]
sample s times = take times $ interpret s

constSignal :: Assertion
constSignal = do
  let signal = constant (10 :: Int)
  sample signal 10 @?= [ 10 | _ <- [1 .. 10] :: [Int] ]

functorSignal :: Assertion
functorSignal = do
  let signal = (+ 1) <$> constant (10 :: Int)
  sample signal 10 @?= [ 11 | _ <- [1 .. 10] :: [Int] ]

applicativeSignal :: Assertion
applicativeSignal = do
  let signal = constant (+ 1) <*> constant (10 :: Int)
  sample signal 10 @?= [ 11 | _ <- [1 .. 10] :: [Int] ]

