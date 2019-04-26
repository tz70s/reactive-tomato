{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}

module Tomato.Colocation.Reactive.Types
  ( Client(..)
  , Clients
  , AppState(..)
  , SourceE(..)
  , Command(..)
  , Context(..)
  , newContext
  , newClient
  , clientRef
  )
where

import Control.Exception
import Data.Unique

import Tomato.Colocation
import Reactive.Tomato as RT

import qualified Data.ByteString.Lazy as BSL
import qualified Network.WebSockets as WS

data Client = C Unique WS.Connection

type Clients = [Client]

instance Eq Client where
  C u1 _ == C u2 _ = u1 == u2

instance Show Client where
  show (C u _) = show $ hashUnique u

data AppState = AppState Clients CurrentView deriving (Eq)

instance Show AppState where
  show (AppState clients view) = show "State : " <> show clients <> ", " <> show view

data SourceE = Add Client | Remove Client | Update Client BSL.ByteString

data Command = BCast AppState | forall e . Exception e => Close e

instance Show Command where
  show (BCast state) = show "Broadcast : " <> show state
  show (Close e    ) = show e

data Context = Context
  { source :: !(EVar SourceE)
  , broker :: !(BVar Command)
  }

-- broker :: !(TVar (Map Client (EVar Command)))
newContext :: IO Context
newContext = Context <$> newEVar <*> newBVar

newClient :: WS.Connection -> IO Client
newClient conn = C <$> newUnique <*> pure conn

clientRef :: Client -> Bref
clientRef = bref . show
