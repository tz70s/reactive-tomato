module Reactive.Tomato.Async
  ( MonadFork(..)
  , Async(..)
  )
where

import           Pipes
import qualified Pipes.Concurrent              as PC
import           Control.Concurrent             ( forkIO
                                                , ThreadId
                                                )
import           Reactive.Tomato.Event
import           Control.Monad                  ( void )

-- Type class for forking thread in a general context.
class MonadFork m where
  fork :: m () -> m ThreadId

instance MonadFork IO where
  fork = forkIO

-- Capable of asynchronous computation acrossing threads.
class Async r where
  async :: r -> r

instance (MonadFork m, MonadIO m) => Async (EventT m a) where
  async = _asyncE

-- Applying the mapping function into separate threads
_asyncE :: (MonadFork m, MonadIO m) => EventT m a -> EventT m a
_asyncE (EventT et) = do
  (output, input) <- liftIO $ PC.spawn PC.unbounded
  void $ lift $ fork $ do
    runEffect $ et >-> PC.toOutput output
    liftIO PC.performGC
  EventT $ PC.fromInput input
