{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Tomato.Colocation.Reactive.Types
  ( Client(..)
  , ControlE(..)
  , DataE(..)
  , Command(..)
  , Context(..)
  , newContext
  , newClient
  , clientRef
  , SIO
  )
where

import           Data.Unique

import           Reactive.Tomato               as RT

import qualified Data.ByteString.Lazy          as BSL
import qualified Network.WebSockets            as WS

data Client = C Unique WS.Connection

instance Eq Client where
  C u1 _ == C u2 _ = u1 == u2

instance Show Client where
  show (C u _) = show $ hashUnique u

data ControlE = Add Client | Remove Client

data DataE = DE Client BSL.ByteString

data Command = BCast BSL.ByteString | Close

data Context = Context
  { cvar :: !(EVar ControlE)
  , dvar :: !(EVar DataE)
  , broker :: !(BVar Command)
  }

newContext :: IO Context
newContext = Context <$> newEVar <*> newEVar <*> newBVar

newClient :: WS.Connection -> IO Client
newClient conn = C <$> newUnique <*> pure conn

clientRef :: Client -> Bref
clientRef = bref . show

-- For simplicity, we use only IO in embedded stack.
type SIO = Signal IO
