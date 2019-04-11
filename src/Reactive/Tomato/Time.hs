module Reactive.Tomato.Time
  ( Timer
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

-- | Spawn a signal that tick every /a/ milliseconds.
every :: MonadIO m => Int -> IO (Timer m)
every intvl = do
  evar <- newEVar
  _    <- forkIO $ go evar
  return $ T (events evar, evar)
 where
  go evar = do
    threadDelay $ intvl * 1000
    emit () evar
    go evar

start :: MonadIO m => Timer m -> Signal m ()
start (T (sig, _)) = sig

-- | Throttle signal propagation speed in milliseconds.
--
-- @
-- let sig = throttle 1000 $ constant 1
-- @
throttle :: MonadIO m => Timer m -> Signal m a -> Signal m a
throttle timer = liftA2 (flip const) (start timer)

-- | Snapshot the value of specific time point.
-- 
-- @
-- let counter = foldp (+) 0 $ constant 1
-- -- tick every one second
-- let tick = every 1000
-- let snap = snapshot tick counter
-- @
--
-- FIXME - current implementation is extremely unuseful,
-- due to lots of performance waste on evaluate source signal.
snapshot :: (MonadFork m, MonadIO m) => Timer m -> Signal m a -> Signal m a
snapshot (T (tick, _)) (Signal p) = do
  -- We'll take one value first then start ticking.
  res <- lift $ next p
  case res of
    Left  _      -> empty
    Right (x, _) -> do
      (output, input) <- liftIO $ PC.spawn $ PC.latest x
      void . lift . fork $ runEffect $ p >-> PC.toOutput output
      liftA2 const (Signal (PC.fromInput input)) tick

window :: MonadIO m => Timer m -> Signal m a -> Signal m (Signal m a)
window tick sig = undefined
