module Reactive.Tomato.Signal.Test
  ( tests
  )
where

import           Test.Tasty
import           Test.Tasty.HUnit
import           Control.Monad.Identity
import qualified Reactive.Tomato.Signal        as Signal
import           Pipes
import qualified Pipes.Prelude                 as Pipes

sig1 :: Signal.Signal Identity Int
sig1 = Signal.constant 10

sig2 :: Signal.Signal Identity Int
sig2 = (+ 1) <$> Signal.constant 10

sig3 :: Signal.Signal Identity Int
sig3 = Signal.constant (+ 1) <*> Signal.constant 10

sample :: Signal.Signal Identity a -> Int -> [a]
sample s times = Pipes.toList $ Signal.runSignal s >-> Pipes.take times

testConstSig :: TestTree
testConstSig = testCase "Constant signal" $ sample sig1 10 @?= [ 10 | _ <- [1 .. 10] ]

testFunctor :: TestTree
testFunctor = testCase "Functor fmapping" $ sample sig2 10 @?= [ 11 | _ <- [1 .. 10] ]

testApplicative :: TestTree
testApplicative = testCase "Applicative apply" $ sample sig3 10 @?= [ 11 | _ <- [1 .. 10] ]

tests :: TestTree
tests = testGroup "Signal Test" [testConstSig, testFunctor, testApplicative]
