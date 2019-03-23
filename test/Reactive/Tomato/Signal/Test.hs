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

sig1 :: Signal Identity Int
sig1 = constant 10

sig2 :: Signal Identity Int
sig2 = (+ 1) <$> constant 10

sig3 :: Signal Identity Int
sig3 = constant (+ 1) <*> constant 10

sample :: Signal Identity a -> Int -> [a]
sample s times = P.toList $ runSignal s >-> P.take times

testConstSig :: TestTree
testConstSig = testCase "Constant signal" $ sample sig1 10 @?= [ 10 | _ <- [1 .. 10] :: [Int] ]

testFunctor :: TestTree
testFunctor = testCase "Functor fmapping" $ sample sig2 10 @?= [ 11 | _ <- [1 .. 10] :: [Int] ]

testApplicative :: TestTree
testApplicative =
  testCase "Applicative apply" $ sample sig3 10 @?= [ 11 | _ <- [1 .. 10] :: [Int] ]

tests :: TestTree
tests = testGroup "Signal Test" [testConstSig, testFunctor, testApplicative]
