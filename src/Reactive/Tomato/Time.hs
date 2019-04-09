module Reactive.Tomato.Time
  ( every
  )
where

import           Control.Concurrent
import           Reactive.Tomato.Signal
import           Reactive.Tomato.EVar
import           Control.Monad.IO.Class

-- | Spawn a signal that tick every /a/ milliseconds.
every :: (MonadIO m, MonadIO m0) => Int -> m0 (Signal m ())
every intvl = do
  evar <- liftIO newEVar
  _    <- liftIO . forkIO $ go evar
  return $ events evar

 where
  go evar = do
    threadDelay $ intvl * 1000
    emit () evar
    go evar
