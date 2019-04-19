{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Tomato.Colocation
  ( RealWorldEvent(..)
  , Colocation
  , Speed
  , EmergencyLevel(..)
  , CurrentView(..)
  , newView
  , updateView
  , deleteView
  , encodeEach
  , collision
  )
where

import Data.Aeson
import Data.Unique
import GHC.Generics (Generic)

import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map as Map
import qualified Data.Text as Text

type Colocation = (Double, Double)
type Speed = Colocation

-- | Domain model for real world event.
data RealWorldEvent = RealWorldEvent
  { location :: !Colocation
  , speed :: !Speed
  , device :: !Text.Text
  , name :: !Text.Text
  } deriving (Eq, Ord, Show, Generic)

instance ToJSON RealWorldEvent
instance FromJSON RealWorldEvent

data EmergencyLevel
  = Low
  | High
  deriving (Eq, Show, Ord, Generic)

instance ToJSON EmergencyLevel
instance FromJSON EmergencyLevel

newtype CurrentView = CurrView (Map.Map Unique RealWorldEvent) deriving (Eq, Ord)

newView :: CurrentView
newView = CurrView Map.empty

deleteView :: Unique -> CurrentView -> CurrentView
deleteView uid (CurrView m) = CurrView $ Map.delete uid m

updateView :: Unique -> RealWorldEvent -> CurrentView -> CurrentView
updateView uid evt (CurrView m) = CurrView $ Map.insert uid evt m

relations :: Unique -> CurrentView -> Maybe EmergencyRelations
relations uid (CurrView m) = do
  value <- Map.lookup uid m
  return $ foldr (go value) [] m
 where
  go v entry xs
    | entry == v = xs
    | otherwise  = EmergencyRelation entry (collision entry v) : xs

data EmergencyRelation = EmergencyRelation RealWorldEvent EmergencyLevel deriving (Show, Generic)

instance ToJSON EmergencyRelation where
  toJSON (EmergencyRelation realE level) = object
    [ "location" .= location realE
    , "speed" .= speed realE
    , "device" .= device realE
    , "name" .= name realE
    , "level" .= level
    ]
instance FromJSON EmergencyRelation

type EmergencyRelations = [EmergencyRelation]

encodeEach :: Unique -> CurrentView -> Maybe BSL.ByteString
encodeEach uid view = do
  rel <- relations uid view
  return $ encode rel

distance :: Colocation -> Colocation -> Double
distance (long1, lat1) (long2, lat2) = sqrt $ (long1 - long2) ** 2 + (lat1 - lat2) ** 2

collision :: RealWorldEvent -> RealWorldEvent -> EmergencyLevel
collision evt1 evt2 = if dist < 10 then High else Low
 where
  loc1 = location evt1
  loc2 = location evt2
  dist = distance loc1 loc2
