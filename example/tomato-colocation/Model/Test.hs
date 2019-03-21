{-# LANGUAGE OverloadedStrings #-}

module Model.Test
  ( testInsert
  )
where

import           Model
import qualified Data.Map                      as Map
import           Data.Unique
import           System.IO.Unsafe

withId :: RealWorldEvent -> IdEvent
withId r = unsafePerformIO $ do
  id <- newUnique
  return (IdEvent id r)

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

testInsert :: Bool
testInsert = updateView newView event1 == (CurrentView $ Map.fromList [(event1, [])])

testInsert2 :: Bool
testInsert2 =
  updateView (updateView newView event1) event2
    == (CurrentView $ Map.fromList [(event1, [(event2, High)]), (event2, [(event1, High)])])

-- Concatenate update should be idempotent.
testInsert3 :: Bool
testInsert3 =
  updateView (updateView (updateView newView event1) event2) event2
    == (CurrentView $ Map.fromList [(event1, [(event2, High)]), (event2, [(event1, High)])])

