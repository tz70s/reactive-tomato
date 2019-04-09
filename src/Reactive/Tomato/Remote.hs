-- | Remote module let you lifting signal into distributed settings.
-- The example use cases include state sharing/replication, load balancing, etc.
-- 
-- == Overview
--
-- Remote preserves signal semantic as well as local settings.

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

{-
  Brain storming: should we make the IO effect in the embedded monad or outside?

  i.e. 
  * @remote :: (MonadIO m, Serialise a) => Sid -> Signal m a@
  * @remote :: Serialise a => Sid -> IO (Signal m a)@

  Seems like second one is nice due to having a flexible embedded monad and clear semantic.
  But how can we approach that?
-}

type Host = String
type PortNum = Integer

data ClusterInfo
  = PubSub { host :: Host, port :: PortNum }
  deriving (Show, Eq)

-- | This can be expanded to multiple cluster method.
newtype ClusterMethod = PubSubM Redis.Connection

newtype Cluster m a
  = CT { unCT :: ReaderT ClusterMethod m a }
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
