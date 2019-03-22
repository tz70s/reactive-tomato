module Reactive.Tomato.Event
  ()
where

import           Pipes
import           Control.Monad                  ( forever )
import qualified Pipes.Concurrent              as PC
import           Control.Concurrent             ( forkIO )
import           Control.Concurrent.STM

-- | EventT is transformer that can produce value across threads.
newtype EventT m a = EventT { unEventT :: Producer a m () }

constE :: (Monad m) => a -> EventT m a
constE a = EventT $ forever $ yield a

forkE :: (MonadIO m, Monad m) => EventT m a -> m (EventT m a)
forkE (EventT et) = do
  (out, input) <- liftIO $ PC.spawn PC.unbounded
  liftIO $ forkIO $ do
    let eff = et >-> PC.toOutput out
    PC.performGC
  return (EventT $ PC.fromInput input)
