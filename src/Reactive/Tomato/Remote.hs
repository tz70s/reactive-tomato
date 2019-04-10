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
-- main = runCluster (PubSub "127.0.0.1" 6379) do
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
-- main = runCluster (PubSub “127.0.0.1” 6379) do
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
  , ClusterInfo(..)
  , Cluster
  , runCluster
  , remote
  , spawn
  )
where

import           Control.Monad                  ( void
                                                , forever
                                                )
import           Control.Concurrent
import           Control.Monad.Reader
import           System.IO
import           Reactive.Tomato.Signal
import           Codec.Serialise
import           Pipes
import           Data.String
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy          as BSL
import qualified Database.Redis                as Redis

-- | Reference identifier for signal, the type variable is useful for type inference.
newtype Sid a = Sid { unSid :: BS.ByteString } deriving (Show)

instance IsString (Sid a) where
  fromString = Sid . fromString

type Host = String
type PortNum = Integer

data ClusterInfo
  = PubSub { host :: Host, port :: PortNum }
  deriving (Show, Eq)

-- | This can be expanded to multiple cluster method.
newtype ClusterMethod = PubSubM Redis.Connection

newtype Cluster m a = CT (ReaderT ClusterMethod m a)
  deriving (Functor, Applicative, Monad, MonadTrans, MonadIO, MonadReader ClusterMethod)

runCluster :: (MonadIO m) => ClusterInfo -> Cluster m a -> m a
runCluster (PubSub _host _port) (CT r) = do
  pool <- liftIO $ Redis.checkedConnect Redis.defaultConnectInfo
    { Redis.connectHost = _host
    , Redis.connectPort = Redis.PortNumber . fromInteger $ _port
    }
  runReaderT r (PubSubM pool)

-- | Create a new remote signal.
--
-- @
-- {-# LANGUAGE OverloadedStrings #-}
-- 
-- main = do
--   sig1 <- remote "sig1"
--   -- use signal can infer the Sid type.
--   -- e.g. we can infer the (Num a) here.
--   sig2 = fmap (+1) sig1
-- @
remote :: (MonadIO m, MonadIO m0, Serialise a) => Sid a -> Cluster m (Signal m0 a)
remote sid = do
  PubSubM conn <- ask
  return $ _subscribe conn sid

spawn :: (MonadIO m, Serialise a) => Sid a -> Signal m a -> Cluster m ()
spawn sid (Signal p) = do
  PubSubM conn <- ask
  lift $ runEffect $ p >-> _publish conn sid

_publish :: (MonadIO m, Serialise a) => Redis.Connection -> Sid a -> Consumer a m ()
_publish conn sid = do
  val <- await
  let serde = BSL.toStrict . serialise $ val
  liftIO $ Redis.runRedis conn $ go sid serde
  where go (Sid chnl) msg = void $ Redis.publish chnl msg

_subscribe :: (MonadIO m, Serialise a) => Redis.Connection -> Sid a -> Signal m a
_subscribe conn (Sid sid) = do
  liftIO $ hSetBuffering stdout LineBuffering
  chan <- liftIO newChan
  _    <- liftIO . forkIO $ Redis.runRedis conn $ Redis.pubSub (Redis.subscribe [sid]) $ \msg -> do
    print msg
    let deserde = deserialiseOrFail . BSL.fromStrict . Redis.msgMessage $ msg
    case deserde of
      Left  ex    -> putStrLn $ "Deserialization failure, cause: " <> show ex
      Right value -> writeChan chan value
    return $ Redis.unsubscribe [sid]
  Signal $ forever $ do
    v <- liftIO $ readChan chan
    Pipes.yield v
