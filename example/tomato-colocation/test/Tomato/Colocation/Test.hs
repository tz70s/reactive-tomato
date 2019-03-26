{-# LANGUAGE OverloadedStrings #-}

module Tomato.Colocation.Test
  ( tests
  )
where

import           Tomato.Colocation
import qualified Data.Map                      as Map
import           Data.Unique
import           System.IO.Unsafe
import           Test.Tasty
import           Test.Tasty.HUnit

tests :: TestTree
tests = testGroup
  "Tomato Colocation Tests"
  [ testCase "Insert from empty map"               insert
  , testCase "Insert two event and build relation" insert2
  , testCase "Insertion should be idempotent"      idempotent
  ]

withId :: RealWorldEvent -> IdEvent
withId r = unsafePerformIO $ do
  _id <- newUnique
  return (IdEvent _id r)

event1 :: IdEvent
event1 = withId $ RealWorldEvent (1, 1) (1, 1) "event1" "event1"

event2 :: IdEvent
event2 = withId $ RealWorldEvent (4, 5) (1, 1) "event2" "event2"

insert :: Assertion
insert = updateView event1 newView @?= (CurrentView $ Map.fromList [(event1, [])])

insert2 :: Assertion
insert2 =
  (updateView event2 . updateView event1) newView
    @?= (CurrentView $ Map.fromList [(event1, [(event2, High)]), (event2, [(event1, High)])])

idempotent :: Assertion
idempotent =
  (updateView event2 . updateView event2 . updateView event1) newView
    @?= (CurrentView $ Map.fromList [(event1, [(event2, High)]), (event2, [(event1, High)])])
