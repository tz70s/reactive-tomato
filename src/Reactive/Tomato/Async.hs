module Reactive.Tomato.Async
  ( MonadFork(..)
  , async
  , merge
  , mergeAll
  )
where

import           Pipes
import           Control.Concurrent      hiding ( yield )
import           Control.Concurrent.STM
import           Control.Monad                  ( void )
import           Control.Monad.Reader

import           Reactive.Tomato.Signal

import qualified Pipes.Concurrent              as PC

-- Type class for forking thread in a general context.
class (Monad m) => MonadFork m where
  fork :: m () -> m ThreadId

instance MonadFork IO where
  fork = forkIO

instance MonadFork m => MonadFork (ReaderT r m) where
  fork (ReaderT r) = ReaderT (fork . r)

-- | Make the wrapping signal into seperate thread.
--
-- @
-- let asyncSignal = async heavySignal
-- @
async :: (MonadFork m, MonadIO m) => Signal m a -> Signal m a
async (Signal as) = do
  (output, input, seal) <- liftIO $ PC.spawn' PC.unbounded
  void $ lift $ fork $ do
    runEffect $ as >-> PC.toOutput output
    liftIO $ atomically seal
  Signal $ PC.fromInput input

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
merge :: (MonadFork m, MonadIO m) => Signal m a -> Signal m a -> Signal m a
merge s1 s2 = mergeAll [s1, s2]

-- | Merge list of signal by interleaving event occurrences.
--
-- There's no guarantee how the interleaving effect is performed.
-- If you need that guarantee, please consider using FRP library.
-- That is, the interleaving effect is non-determinism,
-- the only guarantee is we preserved the FIFO ordering in each signal
mergeAll :: (MonadFork m, MonadIO m) => [Signal m a] -> Signal m a
mergeAll xs = Signal $ do
  (output, input, seal) <- liftIO $ PC.spawn' PC.unbounded
  go output seal xs
  PC.fromInput input
 where
  go _      _    []               = return ()
  go output seal (Signal p : xs') = do
    void . lift . fork $ do
      runEffect $ p >-> PC.toOutput output
      liftIO $ atomically seal
    go output seal xs'
