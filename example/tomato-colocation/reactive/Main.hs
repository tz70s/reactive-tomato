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
import qualified Data.Aeson                    as JSON

clients :: Context -> SIO [Client]
clients Context {..} = foldp go [] (events cvar)
 where
  go (Add    c) xs = c : xs
  go (Remove c) xs = Prelude.filter (/= c) xs

type IdEvent = (Client, RealWorldEvent)

deserialize :: SIO DataE -> SIO IdEvent
deserialize sigdata = filterJust $ do
  (DE client bs) <- sigdata
  case JSON.decode bs of
    Just realE -> return $ Just (client, realE)
    Nothing    -> return Nothing

serialize :: SIO Client -> SIO CurrentView -> SIO BSL.ByteString
serialize sigc sigview = filterJust $ sigview >>= encode
 where
  encode curr = do
    (C uid _) <- sigc
    return $ encodeEach uid curr

currView :: SIO IdEvent -> SIO CurrentView
currView = foldp go newView where go (C uid _, realE) = updateView uid realE

interact' :: Context -> Client -> IO ()
interact' ctx@Context {..} client@(C uid conn) = do
  evar     <- newEVar
  commands <- eventsB (clientRef client) evar broker
  catch (repl commands)
    $ \(e :: WS.ConnectionException) -> putStrLn "Close connection due to client side closed it."

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

  reaction (BCast bs) = putStrLn "Broadcasting data"
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
  WS.runServer "127.0.0.1" 9160 $ handler context
