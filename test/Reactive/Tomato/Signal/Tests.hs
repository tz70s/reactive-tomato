{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Reactive.Tomato.Signal.Tests
  ( tests
  )
where

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck
import           Control.Monad.Identity
import           Reactive.Tomato

tests :: TestTree
tests = testGroup
  "Signal Tests"
  [ testCase "Constant constructor" constSignal
  , testCase "Functor instance"     functorSignal
  , testCase "Applicative instance" applicativeSignal
  , testCase "Correct foldp semantic for implementing counter" folding
  , testProperty "Functor identity law"    prop_functor_identity
  , testProperty "Functor composition law" prop_functor_composition
  ]

instance Show a => Show (Signal Identity a) where
  show = show . interpret

arbitrarySignal :: Arbitrary a => Gen (Signal Identity a)
arbitrarySignal = constant <$> arbitrary

instance Arbitrary a => Arbitrary (Signal Identity a) where
  arbitrary = arbitrarySignal

_sample :: Signal Identity a -> Int -> [a]
_sample s times = take times $ interpret s

constSignal :: Assertion
constSignal = do
  let signal = constant (10 :: Int)
  _sample signal 10 @?= [ 10 | _ <- [1 .. 10] :: [Int] ]

functorSignal :: Assertion
functorSignal = do
  let signal = (+ 1) <$> constant (10 :: Int)
  _sample signal 10 @?= [ 11 | _ <- [1 .. 10] :: [Int] ]

applicativeSignal :: Assertion
applicativeSignal = do
  let signal = constant (+ 1) <*> constant (10 :: Int)
  _sample signal 10 @?= [ 11 | _ <- [1 .. 10] :: [Int] ]

folding :: Assertion
folding = do
  let counter = foldp (\_ s -> s + 1) 0 $ constant ()
  _sample counter 10 @?= ([1 .. 10] :: [Int])

prop_functor_identity :: Int -> Signal Identity Int -> Bool
prop_functor_identity times sig = _sample (fmap id sig) times == _sample sig times

prop_functor_composition :: Int -> Signal Identity Int -> Fun Int Int -> Fun Int Int -> Bool
prop_functor_composition times sig (Fun _ f) (Fun _ g) =
  _sample (fmap (f . g) sig) times == _sample (fmap f . fmap g $ sig) times
