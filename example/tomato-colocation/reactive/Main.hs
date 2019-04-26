{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main
  ( main
  )
where

import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Monad
import Text.Printf (printf)

import Reactive.Tomato as RT
import Tomato.Colocation
import Tomato.Colocation.Reactive.Types

import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Encoding as Text
import qualified Network.WebSockets as WS

appState :: Event SourceE -> IO (Signal AppState)
appState = signal go (AppState [] newView)
 where
  go (Add c) (AppState xs view) = AppState (c : xs) view
  go (Remove c@(C uid _)) (AppState xs view) =
    AppState (Prelude.filter (/= c) xs) (deleteView uid view)
  go (Update (C uid _) bs) (AppState xs view) = case JSON.decode bs of
    Just realEvent -> AppState xs (updateView uid realEvent view)
    Nothing        -> AppState xs view

traceClient :: Event SourceE -> Event (AppState -> (Client, Command))
traceClient es = (\c s -> (c, BCast s)) <$> clients es
 where
  clients es = filterJust $ fmap go es
  go (Update c _) = Just c
  go _            = Nothing

runNetwork :: Context -> IO ()
runNetwork Context {..} = void . forkIO $ do
  (e0, e1) <- RT.duplicate $ events source
  state    <- appState e0
  -- We will perform command only when update event occurs.
  let out = sample state $ traceClient e1
  react out $ \(client, cmd) -> void $ emitB (clientRef client) cmd broker

interact' :: Context -> Client -> IO ()
interact' ctx@Context {..} client@(C uid conn) = do
  Just commands <- derefBVar (clientRef client) broker
  catch (repl commands) $ \(e :: SomeException) -> emit source (Remove client)

 where
  repl commands = do
    _ <- forkSource
    react commands reaction

  -- The exception thrown by this thread need to be handled in main thread in interact'
  -- Hence, we catch close as command and rethrow in the main thread.
  forkSource = forkIO $ catch go $ \e ->
    void $ emitB (clientRef client) (Close (e :: SomeException)) broker
   where
    go = forever $ do
      msg <- WS.receiveData conn
      emit source (Update client msg)

  reaction (BCast (AppState clients view)) =
    forM_ clients $ \(C uid conn) -> forM_ (encodeEach uid view) (WS.sendTextData conn)

  reaction (Close e) = throwIO e

handler :: Context -> WS.PendingConnection -> IO ()
handler ctx@Context {..} pending = do
  conn   <- WS.acceptRequest pending
  client <- newClient conn
  evar   <- newEVar
  register (clientRef client) evar broker
  emit source (Add client)
  WS.forkPingThread conn 30
  interact' ctx client

main :: IO ()
main = do
  let host = "127.0.0.1"
  let port = 9160
  printf "Start websocket server at ws://%s:%d\n" host port
  context <- newContext
  runNetwork context
  WS.runServer host port $ handler context
