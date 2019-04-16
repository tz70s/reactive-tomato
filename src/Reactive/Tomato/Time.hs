module Reactive.Tomato.Time
  ( Timer
  , Time
  , second
  , milli
  , micro
  , every
  , start
  , throttle
  , snapshot
  , window
  )
where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Concurrent      hiding ( yield )
import           Pipes                   hiding ( every )
import qualified Pipes.Concurrent              as PC
import           Reactive.Tomato.Signal
import           Reactive.Tomato.EVar
import           Reactive.Tomato.Async

-- | Necessary to keep EVar because of garbage collection.
newtype Timer m = T (Signal m (), EVar ())

-- | Wrapper for simplify calling with every.
data Time
  = Second Int
  | Milli Int
  | Micro Int

-- | Wrap int into `Time` representation, in second.
second :: Int -> Time
second = Second

-- | Wrap int into `Time` representation, in milliseconds.
milli :: Int -> Time
milli = Milli

-- | Wrap int into `Time` representation, in microseconds.
micro :: Int -> Time
micro = Micro

-- | Spawn a signal that tick every specific time interval.
every :: MonadIO m => Time -> IO (Timer m)
every intvl = do
  evar <- newEVar
  _    <- forkIO $ go evar
  return $ T (events evar, evar)
 where
  toMicro (Second t) = t * 1000000
  toMicro (Milli  t) = t * 1000
  toMicro (Micro  t) = t

  go evar = do
    threadDelay $ toMicro intvl
    emit () evar
    go evar

-- | Start a timer into signal which produce void value.
-- 
-- A common pattern is to use this for throttling or triggering something.
-- 
-- i.e.
-- @
-- main = do
--   timer0 <- every $ second 1
--   -- You get a constant 5 but in every 1 second.
--   let sig0 = 5 <$ start timer0 
-- @
-- 
-- For throttling, you can simply use the @throttle@, which is implemented in:
-- @
-- throttle timer = liftA2 (flip const) (start timer)
-- @
start :: MonadIO m => Timer m -> Signal m ()
start (T (sig, _)) = sig

-- | Throttle signal propagation speed in milliseconds.
--
-- @
-- let sig = throttle (second 1) $ constant 1
-- @
throttle :: MonadIO m => Timer m -> Signal m a -> Signal m a
throttle timer = liftA2 (flip const) (start timer)

-- | Snapshot the value of specific time point.
-- 
-- @
-- let counter = foldp (+) 0 $ constant 1
-- -- tick every one second
-- let tick = every $ second 1
-- let snap = snapshot tick counter
-- @
snapshot :: (MonadFork m, MonadIO m) => Timer m -> Signal m a -> Signal m a
snapshot timer (Signal p) = Signal $ do
  res <- lift $ next p
  case res of
    Left  _      -> pure ()
    Right (x, _) -> do
      (output, input) <- liftIO $ PC.spawn $ PC.latest x
      void . lift . fork $ runEffect $ p >-> PC.toOutput output
      unS $ liftA2 const (Signal (PC.fromInput input)) (start timer)

-- | Window the value of specific time window (interval).
--
-- FIXME - the implementation is not correct.
window :: (MonadFork m, MonadIO m) => Timer m -> Signal m a -> Signal m (Signal m a)
window timer (Signal p) = subroutine <$ start timer
 where
  subroutine = Signal $ do
    (output, input) <- liftIO $ PC.spawn PC.unbounded
    _               <- lift . fork $ runEffect $ p >-> PC.toOutput output
    PC.fromInput input
