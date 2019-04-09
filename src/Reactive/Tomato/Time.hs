module Reactive.Tomato.Time
  ( timeEvery
  )
where

import           Control.Concurrent
import           Reactive.Tomato.Signal
import           Reactive.Tomato.EVar
import           Control.Monad.IO.Class

-- | Spawn a signal that tick every /a/ milliseconds.
timeEvery :: (MonadIO m, MonadIO m0) => Int -> m0 (Signal m Int)
timeEvery intvl = do
  evar <- liftIO newEVar
  _    <- liftIO . forkIO $ go evar 0
  return $ events evar

 where
  go evar expire = do
    threadDelay $ intvl * 1000
    let newExpire = expire + intvl
    emit newExpire evar
    go evar newExpire
