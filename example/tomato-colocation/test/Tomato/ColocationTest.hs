{-# LANGUAGE OverloadedStrings #-}

module Tomato.ColocationTest
  ( tests
  )
where

import           Tomato.Colocation
import qualified Data.Map                      as Map
import           Data.Unique
import           System.IO.Unsafe
import           Test.Tasty
import           Test.Tasty.HUnit

withId :: RealWorldEvent -> IdEvent
withId r = unsafePerformIO $ do
  _id <- newUnique
  return (IdEvent _id r)

event1 :: IdEvent
event1 = withId $ RealWorldEvent (1, 1) (1, 1) "event1" "event1"

event2 :: IdEvent
event2 = withId $ RealWorldEvent (4, 5) (1, 1) "event2" "event2"

showInsert :: String
showInsert = show $ updateView newView event1

showInsert2 :: String
showInsert2 = show $ updateView (updateView newView event1) event2

showInsert3 :: String
showInsert3 = show $ updateView (updateView (updateView newView event1) event2) event2

tests :: TestTree
tests = testGroup
  "Tomato Colocation"
  [ testCase "Insert from empty map"
  $   updateView newView event1
  @?= (CurrentView $ Map.fromList [(event1, [])])
  , testCase "Insert two event and build relation"
  $   updateView (updateView newView event1) event2
  @?= (CurrentView $ Map.fromList [(event1, [(event2, High)]), (event2, [(event1, High)])])
  , testCase "Insertion should be idempotent"
  $   updateView (updateView (updateView newView event1) event2) event2
  @?= (CurrentView $ Map.fromList [(event1, [(event2, High)]), (event2, [(event1, High)])])
  ]
