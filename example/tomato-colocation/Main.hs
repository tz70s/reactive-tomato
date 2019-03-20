{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main
  ( main
  )
where

import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as Text
import qualified Network.WebSockets            as WS
import           Control.Monad                  ( forM_
                                                , forever
                                                )
import           Control.Concurrent.STM.TVar
import           Control.Concurrent.STM
import           Control.Exception              ( try )
import           Control.Monad.State
import           Data.Unique
import           Control.Monad.Except
import           Pipes

type Client = (Unique, WS.Connection)
type Clients = [Client]
type WSState = TVar Clients

newtype WSStateT m a = WSStateT
  { unWSStateT :: StateT WSState m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadState WSState)

newClient :: (MonadIO m) => WS.Connection -> m Client
newClient conn = do
  id <- liftIO newUnique
  return (id, conn)

numClients :: Clients -> Int
numClients = length

addClient :: Client -> Clients -> Clients
addClient client clients = client : clients

removeClient :: Client -> Clients -> Clients
removeClient client = filter ((/= fst client) . fst)

tryE :: (MonadIO m, MonadError WS.ConnectionException m) => IO a -> m a
tryE action = do
  res <- liftIO $ try action
  case res of
    Left  e -> throwError (e :: WS.ConnectionException)
    Right a -> return a

pipeWS
  :: forall m
   . (MonadIO m, MonadState WSState m, MonadError WS.ConnectionException m)
  => WS.Connection
  -> Effect m ()
pipeWS conn = go >-> broadcast
 where
  go :: Producer Text.Text m ()
  go = do
    msg <- tryE $ WS.receiveData conn
    yield msg
    go

-- TODO: possible optimization - current broadcasting relies on single thread.
-- Potential work around is using mailbox in pipes-concurrency and forking work-stealing threads.
broadcast
  :: (MonadIO m, MonadState WSState m, MonadError WS.ConnectionException m)
  => Consumer Text.Text m ()
broadcast = do
  message <- await
  liftIO $ Text.putStrLn $ "Broadcast message : " <> message
  var     <- get
  clients <- liftIO $ readTVarIO var
  tryE $ forM_ clients $ \(id, conn) -> WS.sendTextData conn message
  broadcast

echo :: (MonadIO m, MonadState WSState m) => Client -> m ()
echo client = do
  res <- runExceptT $ runEffect $ pipeWS (snd client)
  case res of
    Left e -> do
      liftIO $ putStrLn $ "Close and remove connection, cause : " <> show
        (e :: WS.ConnectionException)
      -- Close out and remove client connection if any connection exception occurred.
      var <- get
      liftIO $ atomically $ modifyTVar var $ removeClient client
    Right () -> echo client

-- | WS.ServerApp is type of PendingConnection -> IO (), 
-- the model of handling websockets is thread per connection.
-- When the thread return, connection will be automatically closed.
application :: (MonadIO m, MonadState WSState m) => WS.PendingConnection -> m ()
application pending = do
  conn   <- liftIO $ WS.acceptRequest pending
  var    <- get
  client <- newClient conn
  liftIO $ atomically $ modifyTVar var $ addClient client
  liftIO $ putStrLn $ "New connection arrived with id : " <> (show . hashUnique . fst) client
  -- This is for some browser timeout settings (i.e. 60 seconds) to avoid leaking connection.
  liftIO $ WS.forkPingThread conn 30
  echo client

main :: IO ()
main = do
  putStrLn "Start websocket server at ws://127.0.0.1:9160"
  state <- atomically $ newTVar []
  -- TODO: is there any simpler way to integrate transformer stack?
  let handler p = (runStateT . unWSStateT) (application p) state
  let serve = (fmap . fmap) fst handler
  WS.runServer "127.0.0.1" 9160 serve
