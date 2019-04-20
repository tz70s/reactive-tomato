module Reactive.Tomato.Async
  ( MonadAsync(..)
  , asyncS
  , merge
  , mergeAll
  )
where

import Control.Monad.Reader
import Pipes

import Reactive.Tomato.Signal

import qualified Control.Concurrent.Async as Async
import qualified Pipes.Concurrent as PC

-- Type class for forking thread in a general context.
class (Monad m) => MonadAsync m where
  async :: m a -> m (Async.Async a)

instance MonadAsync IO where
  async = Async.async

instance MonadAsync m => MonadAsync (ReaderT r m) where
  async (ReaderT r) = ReaderT (async . r)

-- | Make the wrapping signal into seperate thread.
--
-- @
-- let asyncSignal = forkS heavySignal
-- @
asyncS :: (MonadAsync m, MonadIO m) => Signal m a -> Signal m a
asyncS (Signal as) = Signal $ do
  (output, input) <- liftIO $ PC.spawn PC.unbounded
  a               <- lift $ async $ do
    runEffect $ as >-> PC.toOutput output
    liftIO $ PC.performGC
  PC.fromInput input
  liftIO $ Async.wait a

-- | Merge two signal by interleaving event occurrences.
-- 
-- Comparing to merging in applicative instance, i.e. @liftA2 (+) (constant 1) (constant 2)@,
-- the events are propagated \asynchronously\.
--
-- i.e.
--
-- @
-- let sig1 = foldp (+) 0 $ constant 1
-- let sig2 = foldp (+) 0 $ constant 2
-- let sigm = merge sig1 sig2
-- -- Ideally, the sequence will be interleaved with each signal.
-- -- sigm ~> [1, 2, 2, 3, 4, 4, 6, 5 ..]
-- @
-- 
-- There's no guarantee how the interleaving effect is performed.
-- If you need that guarantee, please consider using FRP library.
-- That is, the interleaving effect is non-determinism,
-- the only guarantee is we preserved the FIFO ordering in each signal.
merge :: (MonadAsync m, MonadIO m) => Signal m a -> Signal m a -> Signal m a
merge s1 s2 = mergeAll [s1, s2]

-- | Merge list of signal by interleaving event occurrences.
--
-- There's no guarantee how the interleaving effect is performed.
-- If you need that guarantee, please consider using FRP library.
-- That is, the interleaving effect is non-determinism,
-- the only guarantee is we preserved the FIFO ordering in each signal
mergeAll :: (Traversable t, MonadAsync m, MonadIO m) => t (Signal m a) -> Signal m a
mergeAll xs = Signal $ do
  (output, input) <- liftIO $ PC.spawn PC.unbounded
  as              <- lift $ forM xs $ \(Signal as) -> async $ do
    runEffect $ as >-> PC.toOutput output
    liftIO $ PC.performGC
  PC.fromInput input
  liftIO $ mapM_ Async.wait as
