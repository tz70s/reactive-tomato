{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Tomato.Colocation.Native.Types
  ( Client(..)
  , Context(..)
  , Env
  , AppM
  , runApp
  , newEnv
  , newClient
  , numClients
  , addClient
  , removeClient
  )
where

import           Control.Monad.Reader
import           Control.Concurrent.STM
import           Data.Unique

import           Tomato.Colocation

import qualified Network.WebSockets            as WS

data Client = C Unique WS.Connection

instance Eq Client where
  C u1 _ == C u2 _ = u1 == u2

instance Show Client where
  show (C u _) = show $ hashUnique u

data Context = S [Client] CurrentView

type Env = TVar Context

newtype AppT m a = A (ReaderT Env m a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Env, MonadTrans)

type AppM = AppT IO

runApp :: AppM a -> Env -> IO a
runApp (A r) = runReaderT r

newEnv :: IO Env
newEnv = newTVarIO $ S [] newView

newClient :: WS.Connection -> IO Client
newClient conn = C <$> newUnique <*> pure conn

numClients :: Context -> Int
numClients (S c _) = length c

addClient :: Client -> Context -> Context
addClient client (S xs view) = S (client : xs) view

removeClient :: Client -> Context -> Context
removeClient c@(C uid _) (S clients view) = S (filter (/= c) clients) $ deleteView uid view
