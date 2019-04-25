-- | Remote module let you lifting signal into distributed settings.
-- The example use cases include state sharing/replication, load balancing, etc.
-- 
-- == Overview
--
-- Remote preserves signal semantic as well as local settings.
-- 
-- Use cases:
--
-- === Case 1 - Distributed Counter
--
-- @
-- {-# LANGUAGE OverloadedStrings #-}
-- 
-- import Reactive.Tomato
-- import Reactive.Tomato.Remote
-- 
-- counter :: Signal m () -> Signal m Int
-- counter = foldp (\_ s -> s + 1) 0
-- 
-- main :: IO ()
-- main = runCluster (Broker "127.0.0.1" 6379) do
--   let cnt = counter $ every 1000
--   -- Spawning counter into remote.
--   spawn "cnt0" cnt
--   -- Retrieving counter from remote.
--   c0 <- remote "cnt0"
-- @
--
-- === Case 2 - Distributed Load Balancing
--
-- @
-- {-# LANGUAGE OverloadedStrings #-}
--
-- import Reactive.Tomato
-- import Reactive.Tomato.Remote
--
-- worker :: (a -> b) -> Signal m a -> Signal m b
-- worker = fmap
--
-- main :: IO ()
-- main = runCluster (Broker “127.0.0.1” 6379) do
--  s0 <- remote “sig0”
--  -- The s0 signal is the start signal
--  -- for driving heavy computation
--  let s1 = worker (1 + fib 200) s0
--  -- evaluate signal...
-- @

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Reactive.Tomato.Remote
  ( Sid
  , sid
  , cancelSid
  , ClusterInfo(..)
  , Cluster
  , runCluster
  , defaultLocal
  , Remote(..)
  )
where

import Codec.Serialise
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.Reader
import Pipes

import Reactive.Tomato.EVar
import Reactive.Tomato.Event
import Reactive.Tomato.Signal

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Database.Redis as Redis

-- | Reference identifier for signal, the type variable is useful for type inference.
data Sid a = Sid { ids :: BS.ByteString, evar :: EVar (Maybe a), associate :: TVar [ThreadId] }

-- | Construct a sid, in cluster monad.
-- 
-- @
-- -- Remember OverloadedStrings extension.
--
-- cluster = do
--   sid0 <- sid "sid0"
-- @
sid :: MonadIO m => BS.ByteString -> Cluster m (Sid a)
sid ids' = do
  evar' <- liftIO newEVar
  ass   <- liftIO $ newTVarIO []
  return Sid { ids = ids', evar = evar', associate = ass }

type Host = String
type PortNum = Integer

data ClusterInfo
  = Broker { host :: Host, port :: PortNum }
  deriving (Show, Eq)

-- | This can be expanded to multiple cluster method.
newtype ClusterMethod = ClusterM Redis.Connection

newtype Cluster m a = CT (ReaderT ClusterMethod m a)
  deriving (Functor, Applicative, Monad, MonadTrans, MonadIO, MonadReader ClusterMethod)

-- | Build monadic computation from configuration, a.k.a ClusterInfo.
--
-- @
-- main = runCluster (Broker "127.0.0.1" 6379) $ do
--   -- computations
-- @
runCluster :: (MonadIO m) => ClusterInfo -> Cluster m a -> m a
runCluster (Broker _host _port) (CT r) = do
  pool <- liftIO $ Redis.checkedConnect Redis.defaultConnectInfo
    { Redis.connectHost = _host
      -- FIXME - we'll fix this deprecation warning when stackage bumps to new hedis version,
      -- then update the stack.yaml resolver.
    , Redis.connectPort = Redis.PortNumber . fromInteger $ _port
    }
  runReaderT r (ClusterM pool)

-- | Helpers for local settings
defaultLocal :: ClusterInfo
defaultLocal = Broker localhost defaultPortNum
 where
  localhost      = "127.0.0.1"
  defaultPortNum = 6379
{-# INLINABLE defaultLocal #-}

-- | Cancel signal.
cancelSid :: MonadIO m => Sid a -> Cluster m ()
cancelSid (Sid _ _ ass) = do
  xs <- liftIO . atomically $ readTVar ass
  liftIO $ forM_ xs $ \tid' -> killThread tid'

-- | Class for remote execution.
class Remote c where
  -- | Create a new remote event or signal.
  --
  -- @
  -- {-# LANGUAGE OverloadedStrings #-}
  -- 
  -- main = runCluster defaultLocal $ do
  --   sig1 <- remote =<< sid "sig1"
  --   -- use signal can infer the Sid type.
  --   -- e.g. we can infer the (Num a) here.
  --   sig2 = fmap (+1) sig1
  --   react sig2 print
  -- @
  remote :: (MonadIO m, Serialise a) => Sid a -> Cluster m (c (Maybe a))

  -- | Spawn a remote event or signal.
  -- 
  -- Note that this is a blocking method, current thread will be blocked until signal terminate.
  -- If you need to make this asynchronous,
  -- the Cluster monad support 'MonadFork' for forking (same as your monad should support it).
  --
  -- @
  -- main = runCluster defaultLocal $ do
  --   timer0 <- every $ milli 10
  --   let sig0 = throttle timer0 $ foldp (+) 0 $ constant 1
  --   -- Spawn a distributed accumulator
  --   sidcnt <- sid "cnt"
  --   -- Note that this will block the thread.
  --   spawn cnt sig0
  -- @
  spawn :: (MonadIO m, Serialise a) => Sid a -> c a -> Cluster m ()

instance Remote Event where
  remote (Sid sid' evar' _) = do
    ClusterM conn <- ask
    _ <- liftIO . forkIO $ Redis.runRedis conn $ Redis.pubSub (Redis.subscribe [sid']) $ \msg -> do
      let deserde = deserialiseOrFail . BSL.fromStrict . Redis.msgMessage $ msg
      case deserde of
        Left  _     -> emit evar' Nothing
        Right value -> emit evar' (Just value)
      return mempty
    return $ events evar'

  spawn (Sid chnl _ ass) (E es) = do
    ClusterM conn <- ask
    tid'          <- liftIO $ forkIO $ runEffect $ es >-> go conn
    liftIO $ atomically $ modifyTVar ass (tid' :)
    return ()
   where
    go conn = do
      val <- await
      let serde = BSL.toStrict . serialise $ val
      _ <- liftIO $ Redis.runRedis conn $ Redis.publish chnl serde
      go conn

{-
instance Remote Signal where
  remote (Sid sid' evar' _) = do
    ClusterM conn <- ask
    _             <- liftIO . forkIO $ Redis.runRedis conn $ do
      receive <- Redis.get sid'

      let
        deserde = case receive of
          Right (Just msg) -> case deserialiseOrFail $ BSL.fromStrict msg of
            Right v -> Just v
            Left  _ -> Nothing
          Right _ -> Nothing
          Left  _ -> Nothing

      liftIO $ emit evar' deserde

    liftIO $ newSignal Nothing (Just <$> filterJust (events evar'))

  spawn (Sid chnl _ ass) sig = do
    ClusterM conn <- ask
    (E es)        <- liftIO $ changes sig
    tid'          <- liftIO $ forkIO $ runEffect $ es >-> go conn
    liftIO $ atomically $ modifyTVar ass (tid' :)
    return ()
   where
    go conn = do
      val <- await
      let serde = BSL.toStrict . serialise $ val
      _ <- liftIO $ Redis.runRedis conn $ Redis.set chnl serde
      go conn
-}
