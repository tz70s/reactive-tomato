{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tomato.Colocation.Tests
  ( tests
  )
where


import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Tomato.Colocation

import qualified Data.Text as Text

tests :: TestTree
tests = testGroup "Tomato Colocation Tests" [testCase "Collision detection function" testCollision]

arbitraryE :: Gen RealWorldEvent
arbitraryE = do
  _location <- arbitrary
  _speed    <- arbitrary
  _device   <- arbitrary
  RealWorldEvent _location _speed (Text.pack _device) . Text.pack <$> arbitrary

instance Arbitrary RealWorldEvent where
  arbitrary = arbitraryE

event1 :: RealWorldEvent
event1 = RealWorldEvent (1, 1) (1, 1) "event1" "event1"

event2 :: RealWorldEvent
event2 = RealWorldEvent (4, 5) (1, 1) "event2" "event2"

testCollision :: Assertion
testCollision = collision event1 event2 @?= High
