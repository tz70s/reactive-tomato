{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main
  ( main
  )
where

import           Control.Applicative
import           Control.Monad
import           Control.Exception
import           Control.Concurrent

import           Reactive.Tomato               as RT
import           Tomato.Colocation
import           Tomato.Colocation.Reactive.Types

import qualified Network.WebSockets            as WS
import qualified Data.ByteString.Lazy          as BSL
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as Text
import qualified Data.Text.Encoding            as Text
import qualified Data.Aeson                    as JSON

clients :: SIO ControlE -> SIO [Client]
clients = foldp go []
 where
  go (Add    c) xs = c : xs
  go (Remove c) xs = Prelude.filter (/= c) xs

type TagEvent = (Client, RealWorldEvent)
type TagBS = (Client, BSL.ByteString)

deserialize :: SIO DataE -> SIO TagEvent
deserialize sigdata = filterJust $ do
  (DE client bs) <- sigdata
  case JSON.decode bs of
    Just realE -> return $ Just (client, realE)
    Nothing    -> return Nothing

-- FIXME - refer to native implementation, the encoding should be done before broadcasting.
serialize :: SIO TagEvent -> SIO CurrentView -> SIO TagBS
serialize sige sigview = filterJust $ sigview >>= encode
 where
  encode curr = do
    (ct@(C uid _), _) <- sige
    return $ (\s -> (ct, s)) <$> encodeEach uid curr

currView :: SIO TagEvent -> SIO CurrentView
currView = foldp go newView where go (C uid _, realE) = updateView uid realE

-- FIXME - buggy.
runNetwork :: Context -> IO ()
runNetwork Context {..} = void . forkIO $ do
  let nrClients = (clients . events) cvar
  -- TODO - we need to duplicate tagEvents for usage below?
  -- Otherwise, view and serde will consumer these by interleaving.
  let tagEvents = (deserialize . events) dvar
  let view      = currView tagEvents
  let serde     = serialize tagEvents view
  react serde $ \(client, bs) -> void $ emitB (clientRef client) (BCast bs) broker

interact' :: Context -> Client -> IO ()
interact' ctx@Context {..} client@(C uid conn) = do
  evar     <- newEVar
  commands <- eventsB (clientRef client) evar broker
  catch (repl commands) $ \(e :: WS.ConnectionException) -> do
    emit (Remove client) cvar
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
      emit (DE client msg) dvar

  reaction (BCast bs) = Text.putStrLn $ "Broadcast data : " <> Text.decodeUtf8 (BSL.toStrict bs)
  reaction (Close e ) = throwIO e

handler :: Context -> WS.PendingConnection -> IO ()
handler ctx@Context {..} pending = do
  conn   <- WS.acceptRequest pending
  client <- newClient conn
  emit (Add client) cvar
  putStrLn $ "New connection arrived with id : " <> show client
  WS.forkPingThread conn 30
  interact' ctx client

main :: IO ()
main = do
  putStrLn "Start websocket server at ws://127.0.0.1:9160"
  context <- newContext
  runNetwork context
  WS.runServer "127.0.0.1" 9160 $ handler context
