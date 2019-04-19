{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Reactive.Tomato.Signal.Tests
  ( tests
  )
where

import Control.Monad.Identity
import Control.Monad.IO.Class
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Reactive.Tomato as RT

tests :: TestTree
tests = testGroup
  "Signal Tests"
  [ testCase "Constant constructor" testConstant
  , testCase "Functor instance"     testFunctor
  , testCase "Applicative instance" testApplicative
  , testCase "Monad instance"       testMonad
  , testCase "Correct foldp semantic for implementing counter" testFolding
  , testProperty "Functor identity law"     prop_functor_identity
  , testProperty "Functor composition law"  prop_functor_composition
  , testProperty "Monad left identity law"  prop_monad_left_identity
  , testProperty "Monad right identity law" prop_monad_right_identity
  , testProperty "Monad associativity law"  prop_monad_associativity
  , testProperty "Correct filter semantic"  prop_filter
  ]

type Sig = Signal Identity

instance Show a => Show (Sig a) where
  show = show . interpret

arbitrarySignal :: Arbitrary a => Gen (Sig a)
arbitrarySignal = constant <$> arbitrary

instance Arbitrary a => Arbitrary (Sig a) where
  arbitrary = arbitrarySignal

_sample :: Sig a -> Int -> [a]
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

testMonad :: Assertion
testMonad = do
  let cnt = foldp (+) 0 $ constant (1 :: Int)
  let
    sigprint = do
      val <- cnt
      liftIO . putStrLn $ "Monad tests get value : " <> show val
      return val
  xs <- interpretM $ RT.take 10 sigprint
  xs @?= [1 .. 10]

testFolding :: Assertion
testFolding = do
  let counter = foldp (\_ s -> s + 1) 0 $ constant ()
  _sample counter 10 @?= ([1 .. 10] :: [Int])

prop_functor_identity :: Int -> Sig Int -> Bool
prop_functor_identity times sig = _sample (fmap id sig) times == _sample sig times

prop_functor_composition :: Int -> Sig Int -> Fun Int Int -> Fun Int Int -> Bool
prop_functor_composition times sig (Fun _ f) (Fun _ g) =
  _sample (fmap (f . g) sig) times == _sample (fmap f . fmap g $ sig) times

prop_monad_left_identity :: Int -> Int -> Fun Int (Sig Int) -> Bool
prop_monad_left_identity times num (Fun _ f) =
  _sample (return num >>= f) times == _sample (f num) times

prop_monad_right_identity :: Int -> Sig Int -> Bool
prop_monad_right_identity times sig = _sample (sig >>= return) times == _sample sig times

prop_monad_associativity :: Int -> Sig Int -> Fun Int (Sig Int) -> Fun Int (Sig Int) -> Bool
prop_monad_associativity times sig (Fun _ f) (Fun _ g) =
  _sample ((sig >>= f) >>= g) times == _sample (sig >>= (\x -> f x >>= g)) times

prop_filter :: Int -> Bool
prop_filter times = _sample (RT.filter (\s -> s `mod` 2 == 0) cnt) times
  == Prelude.take times ([2, 4 ..] :: [Int])
  where cnt = foldp (\_ s -> s + 1) 0 $ constant ()
