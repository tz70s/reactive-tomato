{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tomato.Colocation.Tests
  ( tests
  )
where

import           Tomato.Colocation
import qualified Data.Map                      as Map
import           Data.Unique
import           System.IO.Unsafe
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck
import           Data.Text                     as Text

tests :: TestTree
tests = testGroup
  "Tomato Colocation Tests"
  [ testProperty "Insert from empty map" prop_insert
--  , testProperty "Replace event when insert multiple times" prop_replace
  , testCase "Insert two event and build relation" insert2
  , testCase "Insertion should be idempotent"      idempotent
  ]

arbitraryE :: Gen IdEvent
arbitraryE = do
  let _id = unsafePerformIO newUnique
  _location <- arbitrary
  _speed    <- arbitrary
  _device   <- arbitrary
  IdEvent _id . RealWorldEvent _location _speed (Text.pack _device) . Text.pack <$> arbitrary

instance Arbitrary IdEvent where
  arbitrary = arbitraryE

withId :: RealWorldEvent -> IdEvent
withId r = unsafePerformIO $ do
  _id <- newUnique
  return (IdEvent _id r)

event1 :: IdEvent
event1 = withId $ RealWorldEvent (1, 1) (1, 1) "event1" "event1"

event2 :: IdEvent
event2 = withId $ RealWorldEvent (4, 5) (1, 1) "event2" "event2"

prop_insert :: IdEvent -> Bool
prop_insert evt = updateView evt newView == (CurrentView $ Map.fromList [(evt, [])])

-- TODO: current replacement is not correct.
prop_replace :: Int -> IdEvent -> Bool
prop_replace times evt =
  Prelude.foldr (\_ view -> updateView evt view) newView [1 .. times]
    == (CurrentView $ Map.fromList [(evt, [])])

insert2 :: Assertion
insert2 =
  (updateView event2 . updateView event1) newView
    @?= (CurrentView $ Map.fromList [(event1, [(event2, High)]), (event2, [(event1, High)])])

idempotent :: Assertion
idempotent =
  (updateView event2 . updateView event2 . updateView event1) newView
    @?= (CurrentView $ Map.fromList [(event1, [(event2, High)]), (event2, [(event1, High)])])
