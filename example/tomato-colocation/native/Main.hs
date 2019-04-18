{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Main
  ( main
  )
where

import           Control.Exception
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.Except
import           Control.Concurrent             ( forkIO )
import           Control.Concurrent.STM

import           Tomato.Colocation
import           Tomato.Colocation.Native.Types

import qualified Data.ByteString.Lazy          as BSL
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as Text
import qualified Data.Aeson                    as JSON
import qualified Network.WebSockets            as WS

rethrow :: IO a -> ExceptT WS.ConnectionException AppM a
rethrow action = do
  res <- liftIO $ try action
  case res of
    Left  e -> throwError e
    Right r -> return r

interact' :: Client -> AppM ()
interact' client = do
  res <- runExceptT (process client)
  case res of
    Left e -> do
      liftIO $ putStrLn $ "Connection close, remove client " <> show client <> "."
      -- Close out and remove client connection if any connection exception occurred.
      env <- ask
      liftIO $ atomically $ modifyTVar env $ removeClient client
    Right _ -> interact' client

process :: Client -> ExceptT WS.ConnectionException AppM ()
process (C uid conn) = forever $ do
  msg <- rethrow $ WS.receiveData conn
  case JSON.decode msg of
    Just m -> update m >>= (rethrow . broadcast)
    Nothing ->
      liftIO $ putStrLn "Wrong real world event format, currently simply abort this message."
 where
  update realE = do
    env <- ask
    liftIO . atomically $ do
      (S clients view) <- readTVar env
      let updated = S clients $ updateView uid realE view
      writeTVar env updated
      return updated

  broadcast (S clients view) =
    forM_ clients $ \(C uid conn) -> forM_ (encodeEach uid view) (WS.sendTextData conn)

-- | WS.ServerApp is type of PendingConnection -> IO (), 
-- the model of handling websockets is thread per connection.
-- When the thread return, connection will be automatically closed.
handler :: WS.PendingConnection -> AppM ()
handler pending = do
  conn   <- liftIO $ WS.acceptRequest pending
  env    <- ask
  client <- liftIO $ newClient conn
  liftIO $ atomically $ modifyTVar env $ addClient client
  liftIO $ putStrLn $ "New connection arrived with id : " <> show client
  -- This is for some browser timeout settings (i.e. 60 seconds) to avoid leaking connection.
  liftIO $ WS.forkPingThread conn 30
  interact' client

main :: IO ()
main = do
  putStrLn "Start websocket server at ws://127.0.0.1:9160"
  env <- newEnv
  let serve = fmap (`runApp` env) handler
  WS.runServer "127.0.0.1" 9160 serve
