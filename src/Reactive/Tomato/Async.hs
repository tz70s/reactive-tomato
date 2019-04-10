module Reactive.Tomato.Async
  ( MonadFork(..)
  , async
  , interl
  )
where

import           Pipes
import qualified Pipes.Concurrent              as PC
import           Control.Concurrent      hiding ( yield )
import           Reactive.Tomato.Signal
import           Control.Monad                  ( void )
import           System.IO.Unsafe

-- Type class for forking thread in a general context.
class (Monad m) => MonadFork m where
  fork :: m () -> m ThreadId

instance MonadFork IO where
  fork = forkIO

-- | Make the wrapping signal into seperate thread.
--
-- @
-- let asyncSignal = async heavySignal
-- @
async :: (MonadFork m, MonadIO m) => Signal m a -> Signal m a
async (Signal as) = do
  (output, input) <- liftIO $ PC.spawn PC.unbounded
  void $ lift $ fork $ do
    runEffect $ as >-> PC.toOutput output
    liftIO PC.performGC
  Signal $ PC.fromInput input

interl :: (MonadFork m, MonadIO m) => Signal m a -> Signal m a -> Signal m a
interl (Signal p1) (Signal p2) = Signal $ do
  (output, input) <- liftIO $ PC.spawn PC.unbounded
  _               <- lift . fork $ do
    runEffect $ p1 >-> PC.toOutput output
    liftIO PC.performGC
  _ <- lift . fork $ do
    runEffect $ p2 >-> PC.toOutput output
    liftIO PC.performGC
  PC.fromInput input

-- TODO: how to implment this?
unsafeInterl :: (Monad m) => Signal m a -> Signal m a -> Signal m a
unsafeInterl (Signal p1) (Signal p2) = undefined
