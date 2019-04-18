{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main
  ( main
  )
where

import           Control.Exception
import           Control.Monad                  ( forM_
                                                , forever
                                                )
import           Control.Monad.Reader
import           Control.Monad.Except
import           Control.Concurrent             ( forkIO )
import           Control.Concurrent.STM

import           Data.Unique
import qualified Data.ByteString.Lazy          as BSL
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as Text
import qualified Data.Aeson                    as JSON

import qualified Network.WebSockets            as WS
import           Tomato.Colocation

data Client = C Unique WS.Connection

instance Eq Client where
  C u1 _ == C u2 _ = u1 == u2

instance Show Client where
  show (C u _) = show $ hashUnique u

data StateA = S [Client] CurrentView

type Env = TVar StateA

newtype AppT m a = A
  { unA :: ReaderT Env m a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadReader Env, MonadTrans)

instance MonadError e m => MonadError e (AppT m) where
  throwError = lift . throwError
  catchError (A a) f = A $ catchError a (fmap unA f)

type AppM = AppT IO

newEnv :: IO Env
newEnv = newTVarIO $ S [] newView

newClient :: WS.Connection -> IO Client
newClient conn = C <$> newUnique <*> pure conn

numClients :: StateA -> Int
numClients (S c _) = length c

addClient :: Client -> StateA -> StateA
addClient client (S xs view) = S (client : xs) view

removeClient :: Client -> StateA -> StateA
removeClient c@(C uid _) (S clients view) = S (filter (/= c) clients) $ deleteView uid view

rethrow :: IO a -> ExceptT WS.ConnectionException AppM a
rethrow action = do
  res <- liftIO $ try action
  case res of
    Left e -> throwError e
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
  let serve = fmap (\ma -> runReaderT (unA ma) env) handler
  WS.runServer "127.0.0.1" 9160 serve
