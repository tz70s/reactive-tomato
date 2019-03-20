{-# LANGUAGE DeriveGeneric #-}

module Model
  ( RealWorldEvent(..)
  , Colocation
  , Speed
  )
where

import           Data.Aeson
import           GHC.Generics                   ( Generic )
import qualified Data.Text                     as Text

type Colocation = (Double, Double)
type Speed = Colocation

-- | Domain model for real world event.
data RealWorldEvent = RealWorldEvent
  { location :: !Colocation
  , speed :: !Speed
  , device :: !Text.Text
  , name :: !Text.Text
  } deriving (Eq, Show, Generic)

instance ToJSON RealWorldEvent
instance FromJSON RealWorldEvent
