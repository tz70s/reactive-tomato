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

import Reactive.Tomato as RT
import Tomato.Colocation
import Tomato.Colocation.Reactive.Types

import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Encoding as Text
import qualified Network.WebSockets as WS

clients :: Event ControlE -> Event [Client]
clients = foldp go []
 where
  go (Add    c) xs = c : xs
  go (Remove c) xs = Prelude.filter (/= c) xs

type TagEvent = (Client, RealWorldEvent)
type TagBS = (Client, BSL.ByteString)

deserialize :: Event DataE -> Event TagEvent
deserialize sigdata = filterJust $ do
  (DE client bs) <- sigdata
  case JSON.decode bs of
    Just realE -> return $ Just (client, realE)
    Nothing    -> return Nothing

currView :: Event TagEvent -> Event CurrentView
currView = foldp go newView where go (C uid _, realE) = updateView uid realE

runNetwork :: Context -> IO ()
runNetwork Context {..} = void . forkIO $ do
  clientSignal <- newSignal [] $ (clients . events) cvar
  let tagEvents = (deserialize . events) dvar
  let view      = liftA2 (,) tagEvents (currView tagEvents)
  react view $ \((client, _), view) -> void $ emitB (clientRef client) (BCast view) broker

interact' :: Context -> Client -> IO ()
interact' ctx@Context {..} client@(C uid conn) = do
  evar     <- newEVar
  commands <- eventsB (clientRef client) evar broker
  catch (repl commands) $ \(e :: SomeException) -> do
    emit cvar (Remove client)
    putStrLn "Close connection due to client side closed it."

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
      emit dvar (DE client msg)

  reaction (BCast view) = case encodeEach uid view of
    Just bs -> Text.putStrLn $ "Broadcast data : " <> Text.decodeUtf8 (BSL.toStrict bs)
    Nothing -> return ()
  reaction (Close e) = throwIO e

handler :: Context -> WS.PendingConnection -> IO ()
handler ctx@Context {..} pending = do
  conn   <- WS.acceptRequest pending
  client <- newClient conn
  emit cvar (Add client)
  putStrLn $ "New connection arrived with id : " <> show client
  WS.forkPingThread conn 30
  interact' ctx client

main :: IO ()
main = do
  putStrLn "Start websocket server at ws://127.0.0.1:9160"
  context <- newContext
  runNetwork context
  WS.runServer "127.0.0.1" 9160 $ handler context
