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
import           Prelude               hiding (filter)

tests :: TestTree
tests = testGroup
  "Signal Tests"
  [ testCase "Constant constructor" testConstant
  , testCase "Functor instance"     testFunctor
  , testCase "Applicative instance" testApplicative
  , testCase "Correct foldp semantic for implementing counter" testFolding
  , testProperty "Functor identity law"     prop_functor_identity
  , testProperty "Functor composition law"  prop_functor_composition
  , testProperty "Correct filter semantic" prop_filter
  ]

instance Show a => Show (Signal Identity a) where
  show = show . interpret

arbitrarySignal :: Arbitrary a => Gen (Signal Identity a)
arbitrarySignal = constant <$> arbitrary

instance Arbitrary a => Arbitrary (Signal Identity a) where
  arbitrary = arbitrarySignal

_sample :: Signal Identity a -> Int -> [a]
_sample s times = Prelude.take times $ interpret s

testConstant :: Assertion
testConstant = do
  let signal = constant (10 :: Int)
  _sample signal 10 @?= [ 10 | _ <- [1 .. 10] :: [Int] ]

testFunctor :: Assertion
testFunctor = do
  let signal = (+ 1) <$> constant (10 :: Int)
  _sample signal 10 @?= [ 11 | _ <- [1 .. 10] :: [Int] ]

testApplicative :: Assertion
testApplicative = do
  let signal = constant (+ 1) <*> constant (10 :: Int)
  _sample signal 10 @?= [ 11 | _ <- [1 .. 10] :: [Int] ]

testFolding :: Assertion
testFolding = do
  let counter = foldp (\_ s -> s + 1) 0 $ constant ()
  _sample counter 10 @?= ([1 .. 10] :: [Int])

prop_functor_identity :: Int -> Signal Identity Int -> Bool
prop_functor_identity times sig = _sample (fmap id sig) times == _sample sig times

prop_functor_composition :: Int -> Signal Identity Int -> Fun Int Int -> Fun Int Int -> Bool
prop_functor_composition times sig (Fun _ f) (Fun _ g) =
  _sample (fmap (f . g) sig) times == _sample (fmap f . fmap g $ sig) times

prop_filter :: Int -> Bool
prop_filter times = _sample (filter (\s -> s `mod` 2 == 0) cnt) times
  == Prelude.take times ([2, 4 ..] :: [Int])
  where cnt = foldp (\_ s -> s + 1) 0 $ constant ()
