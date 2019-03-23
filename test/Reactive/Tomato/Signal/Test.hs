module Reactive.Tomato.Signal.Test
  ( tests
  )
where

import           Test.Tasty
import           Test.Tasty.HUnit
import           Control.Monad.Identity
import           Reactive.Tomato                ( Signal
                                                , constant
                                                , runSignal
                                                )
import           Pipes
import qualified Pipes.Prelude                 as P

tests :: TestTree
tests = testGroup
  "Signal Tests"
  [ testCase "Constant constructor" constSignal
  , testCase "Functor instance"     functorSignal
  , testCase "Applicative instance" applicativeSignal
  ]

sample :: Signal Identity a -> Int -> [a]
sample s times = P.toList $ runSignal s >-> P.take times

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

