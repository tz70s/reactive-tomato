{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Reactive.Tomato.Event.Tests
  ( tests
  )
where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck hiding (generate)

import Reactive.Tomato as RT

tests :: TestTree
tests = testGroup
  "Event Tests"
  [ testCase "Constant constructor" testConstant
  , testCase "Functor instance"     testFunctor
  , testCase "Applicative instance" testApplicative
  , testCase "Monad instance"       testMonad
  , testCase "Correct foldp semantic for implementing counter" testFolding
  , testCase "Schedule event processing into separate thread" testSchedule
  , testCase "Duplicate events"     testDuplicate
  ]

arbitrarySignal :: Arbitrary a => Gen (Event a)
arbitrarySignal = RT.repeat <$> arbitrary

instance Arbitrary a => Arbitrary (Event a) where
  arbitrary = arbitrarySignal

_sample :: Event a -> Int -> IO [a]
_sample es times = interpret (RT.take times es)

testConstant :: Assertion
testConstant = do
  let e0 = RT.repeat (10 :: Int)
  xs <- _sample e0 10
  xs @?= [ 10 | _ <- [1 .. 10] :: [Int] ]

testFunctor :: Assertion
testFunctor = do
  let e0 = (+ 1) <$> RT.repeat (10 :: Int)
  xs <- _sample e0 10
  xs @?= [ 11 | _ <- [1 .. 10] :: [Int] ]

testApplicative :: Assertion
testApplicative = do
  let e0 = RT.repeat (+ 1) <*> RT.repeat (10 :: Int)
  xs <- _sample e0 10
  xs @?= [ 11 | _ <- [1 .. 10] :: [Int] ]

testMonad :: Assertion
testMonad = do
  let cnt    = foldp (+) 0 $ RT.repeat (1 :: Int)
  let prints = cnt >>= return
  xs <- interpret $ RT.take 10 prints
  xs @?= [1 .. 10]

testFolding :: Assertion
testFolding = do
  let counter = foldp (\_ s -> s + 1) 0 $ RT.repeat ()
  xs <- _sample counter 10
  xs @?= ([1 .. 10] :: [Int])

testSchedule :: Assertion
testSchedule = do
  let e0 = generate [1 ..]
  let e1 = schedule e0
  xs <- _sample e1 10
  xs @?= ([1 .. 10] :: [Int])

testDuplicate :: Assertion
testDuplicate = do
  let e0 = generate [1 ..]
  (e1, e2) <- duplicate e0
  xs1      <- _sample e1 10
  xs2      <- _sample e2 10
  xs1 @?= ([1 .. 10] :: [Int])
  xs2 @?= ([1 .. 10] :: [Int])
