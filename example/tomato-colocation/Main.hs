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
import           Model
import qualified Data.Aeson                    as JSON
import qualified Data.ByteString.Lazy          as BSL

type Client = (Unique, WS.Connection)
data Clients = Clients [Client] CurrentView
type WSState = TVar Clients

newtype WSStateT m a = WSStateT
  { unWSStateT :: StateT WSState m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadState WSState)

newClient :: (MonadIO m) => WS.Connection -> m Client
newClient conn = do
  id <- liftIO newUnique
  return (id, conn)

numClients :: Clients -> Int
numClients (Clients c _) = length c

addClient :: Client -> Clients -> Clients
addClient client (Clients clients c) = Clients (client : clients) c

removeClient :: Client -> Clients -> Clients
removeClient (id, _) (Clients clients view) =
  Clients (filter ((/= id) . fst) clients) $ deleteView view id

tryE :: (MonadIO m, MonadError WS.ConnectionException m) => IO a -> m a
tryE action = do
  res <- liftIO $ try action
  case res of
    Left  e -> throwError (e :: WS.ConnectionException)
    Right a -> return a

pipeWS
  :: forall m
   . (MonadIO m, MonadState WSState m, MonadError WS.ConnectionException m)
  => Client
  -> Effect m ()
pipeWS (id, conn) = go >-> broadcast
 where
  go :: Producer IdEvent m ()
  go = do
    msg <- tryE $ WS.receiveData conn
    let evt = JSON.decode msg
    case evt of
      Just m  -> yield (IdEvent id m) >> go
      Nothing -> liftIO (putStrLn "Wrong real world event format, currently simply abort this message.") >> go

-- TODO: possible optimization - current broadcasting relies on single thread.
-- Potential work around is using mailbox in pipes-concurrency and forking work-stealing threads.
broadcast
  :: (MonadIO m, MonadState WSState m, MonadError WS.ConnectionException m) => Consumer IdEvent m ()
broadcast = do
  event                  <- await
  var                    <- get
  (Clients clients view) <- liftIO $ atomically $ go var event
  tryE $ forM_ clients $ \(id, conn) -> WS.sendTextData conn $ encodeEach view id
  broadcast
 where
  go var event = do
    modifyTVar var (\(Clients clients view) -> Clients clients $ updateView view event)
    readTVar var

talk :: (MonadIO m, MonadState WSState m) => Client -> m ()
talk client = do
  res <- runExceptT $ runEffect $ pipeWS client
  case res of
    Left e -> do
      liftIO $ putStrLn $ "Close and remove connection, cause : " <> show
        (e :: WS.ConnectionException)
      -- Close out and remove client connection if any connection exception occurred.
      var <- get
      liftIO $ atomically $ modifyTVar var $ removeClient client
    Right () -> talk client

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
  talk client

main :: IO ()
main = do
  putStrLn "Start websocket server at ws://127.0.0.1:9160"
  state <- atomically $ newTVar (Clients [] newView)
  -- TODO: is there any simpler way to integrate transformer stack?
  let handler p = (runStateT . unWSStateT) (application p) state
  let serve = (fmap . fmap) fst handler
  WS.runServer "127.0.0.1" 9160 serve
