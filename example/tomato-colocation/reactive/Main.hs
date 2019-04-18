{-# LANGUAGE RecordWildCards #-}

module Main
  ( main
  )
where

import qualified Network.WebSockets            as WS
import           Data.Unique
import           Reactive.Tomato
import qualified Data.Set                      as Set
import qualified Data.ByteString.Lazy          as BSL

type Set = Set.Set
type Client = (Unique, WS.Connection)

data Context = Context
  { cvar :: !(EVar Client)
  , dvar :: !(EVar BSL.ByteString)
  }

newContext :: IO Context
newContext = Context <$> newEVar <*> newEVar

newClient :: WS.Connection -> IO Client
newClient conn = do
  id <- newUnique
  return (id, conn)

-- For simplicity, we use only IO in embedded stack.
type SIO = Signal IO

clients :: SIO (Set Client)
clients = constant Set.empty

handler :: Context -> WS.PendingConnection -> IO ()
handler Context {..} pending = do
  conn   <- WS.acceptRequest pending
  client <- newClient conn
  emit client cvar
  putStrLn $ "New connection arrived with id : " <> (show . hashUnique . fst) client
  -- This is for some browser timeout settings (i.e. 60 seconds) to avoid leaking connection.
  WS.forkPingThread conn 30
  -- Await events for reactions.

main :: IO ()
main = do
  putStrLn "Start websocket server at ws://127.0.0.1:9160"
  context <- newContext
  WS.runServer "127.0.0.1" 9160 (handler context)
