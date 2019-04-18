{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main
  ( main
  )
where

import qualified Network.WebSockets            as WS
import           Data.Unique
import           Reactive.Tomato               as RT
import qualified Data.ByteString.Lazy          as BSL
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as Text
import           Control.Applicative
import           Control.Monad
import           Control.Exception

data Client = C Unique WS.Connection

instance Eq Client where
  C u1 _ == C u2 _ = u1 == u2

instance Show Client where
  show (C u _) = show $ hashUnique u

data ClientE = Add Client | Remove Client

data Command = BCast BSL.ByteString | Close

data Context = Context
  { cvar :: !(EVar ClientE)
  , dvar :: !(EVar BSL.ByteString)
  }

newContext :: IO Context
newContext = Context <$> newEVar <*> newEVar

newClient :: WS.Connection -> IO Client
newClient conn = C <$> newUnique <*> pure conn

-- For simplicity, we use only IO in embedded stack.
type SIO = Signal IO

clients :: Context -> SIO [Client]
clients Context {..} = foldp go [] (events cvar)
 where
  go (Add    c) xs = c : xs
  go (Remove c) xs = Prelude.filter (/= c) xs

handler :: Context -> WS.PendingConnection -> IO ()
handler ctx@Context {..} pending = do
  conn   <- WS.acceptRequest pending
  client <- newClient conn
  emit (Add client) cvar
  putStrLn $ "New connection arrived with id : " <> show client
  WS.forkPingThread conn 30
  catch (react carrier reaction) $ \(e :: WS.ConnectionException) -> emit (Remove client) cvar

 where
  isme me = RT.filter extract (events cvar)
   where
    extract (Add    c) = c == me
    extract (Remove c) = c == me

  carrier = liftA2 (,) (events cvar) (clients ctx)

  reaction (c, xs) = do
    let text (Add    c') = Text.pack $ "New client : " <> show c'
        text (Remove c') = Text.pack $ "Remove client : " <> show c'
    Text.putStrLn $ "Broadcast Text - " <> text c
    forM_ xs $ \(C _ conn) -> WS.sendTextData conn (text c)

main :: IO ()
main = do
  putStrLn "Start websocket server at ws://127.0.0.1:9160"
  context <- newContext
  WS.runServer "127.0.0.1" 9160 (handler context)
