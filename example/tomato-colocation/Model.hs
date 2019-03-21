{-# LANGUAGE DeriveGeneric #-}

module Model
  ( RealWorldEvent(..)
  , IdEvent(..)
  , Colocation
  , Speed
  , EmergencyLevel(..)
  , CurrentView(..)
  , newView
  , updateView
  , deleteView
  , encodeEach
  )
where

import           Data.Aeson
import           GHC.Generics                   ( Generic )
import qualified Data.Text                     as Text
import qualified Data.Map                      as Map
import           Data.Unique
import qualified Data.ByteString.Lazy          as BSL

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

data IdEvent = IdEvent Unique RealWorldEvent

instance Eq IdEvent where
  (IdEvent l _) == (IdEvent r _) = l == r

instance Show IdEvent where
  show (IdEvent l r) = "IdEvent (" <> (show . hashUnique) l <> ", " <> show r <> ")"

instance Ord IdEvent where
  compare (IdEvent l _) (IdEvent r _) = compare l r

data EmergencyLevel
  = Low
  | High
  deriving (Eq, Show, Ord, Generic)

instance ToJSON EmergencyLevel
instance FromJSON EmergencyLevel

type EmergencyRelation = (IdEvent, EmergencyLevel)
type EmergencyRelations = [EmergencyRelation]

newtype CurrentView = CurrentView { unCurrView :: Map.Map IdEvent EmergencyRelations} deriving (Eq, Show, Generic)

type JSONView = Map.Map RealWorldEvent [(RealWorldEvent, EmergencyLevel)]

encodeEach :: CurrentView -> Unique -> BSL.ByteString
encodeEach (CurrentView view) id =
  (encode . concat . trim . Map.elems . Map.filterWithKey (\(IdEvent id' _) _ -> id' == id)) view
  where trim = (fmap . fmap) (\(IdEvent _ evt, level) -> (evt, level))

newView :: CurrentView
newView = CurrentView Map.empty

deleteView :: CurrentView -> Unique -> CurrentView
deleteView (CurrentView view) id = CurrentView $ Map.map go $ Map.filterWithKey
  (\(IdEvent id' _) _ -> id' /= id)
  view
  where go = filter (\(IdEvent id' _, _) -> id' /= id)

-- | TODO: optimize & simplify, current algorithm is inefficient and too complex.
updateView :: CurrentView -> IdEvent -> CurrentView
updateView (CurrentView view) event =
  CurrentView
    $  Map.fromList
         [ (k, v')
         | (k, v) <- Map.toList view
         , k /= event
         , let v' = if checkExist event v
                 then replace event k v
                 else v <> [(event, collision k event)]
         ]
    <> insertIfNotExist event view

-- Helper functions for update.

insertIfNotExist
  :: IdEvent -> Map.Map IdEvent EmergencyRelations -> Map.Map IdEvent EmergencyRelations
insertIfNotExist event view = Map.fromList
  [ ( event
    , [ (evt, level) | (evt, _) <- Map.toList view, evt /= event, let level = collision evt event ]
    )
  ]

-- | Check whether event is existed in relations, used for guard insertion.
checkExist :: IdEvent -> EmergencyRelations -> Bool
checkExist evt = any (\r -> evt == fst r)

-- | Replace emergency value in relations.
replace :: IdEvent -> IdEvent -> EmergencyRelations -> EmergencyRelations
replace src dst rels =
  [ r | x <- rels, let r = if fst x == src then (src, collision src dst) else x ]

distance :: Colocation -> Colocation -> Double
distance (long1, lat1) (long2, lat2) = sqrt $ (long1 - long2) ** 2 + (lat1 - lat2) ** 2

collision :: IdEvent -> IdEvent -> EmergencyLevel
collision (IdEvent _ evt1) (IdEvent _ evt2) = if dist < 10 then High else Low
 where
  loc1 = location evt1
  loc2 = location evt2
  dist = distance loc1 loc2
