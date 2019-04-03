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
import           Reactive.Tomato.Signal
import           Control.Monad                  ( void )

-- Type class for forking thread in a general context.
class (Monad m) => MonadFork m where
  fork :: m () -> m ThreadId

instance MonadFork IO where
  fork = forkIO

-- Capable of asynchronous computation acrossing threads.
class Async r where
  async :: r -> r

instance (MonadFork m, MonadIO m) => Async (Signal m a) where
  async = _async

-- | Make the wrapping signal into seperate thread.
--
-- @
-- let asyncSignal = async heavySignal
-- @
_async :: (MonadFork m, MonadIO m) => Signal m a -> Signal m a
_async (Signal as) = do
  (output, input) <- liftIO $ PC.spawn PC.unbounded
  void $ lift $ fork $ do
    runEffect $ as >-> PC.toOutput output
    liftIO PC.performGC
  Signal $ PC.fromInput input
