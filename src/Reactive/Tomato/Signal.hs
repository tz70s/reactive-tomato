{-# LANGUAGE ScopedTypeVariables #-}

module Reactive.Tomato.Signal
  ( Signal(..)
  , newSignal
  , cancel
  , changes
  )
where

import Control.Concurrent (forkIO, ThreadId, killThread)
import Control.Concurrent.STM

import Pipes
import Reactive.Tomato.EVar
import Reactive.Tomato.Event

-- | Now idea to close the thread id properly.
data Signal a = Signal { cache :: TVar a, tid :: ThreadId }

-- Thought: we can react to pump, change tha cache value and propagate to latches.
-- Maybe we can return IO (Signal a), fork a thread and use it to generate events.
-- ThreadId will be cached then we can kill it when we need?
newSignal :: a -> Event a -> IO (Signal a)
newSignal initial pump' = do
  cache' <- newTVarIO initial
  tid'   <- forkIO $ react pump' (reaction cache')
  return (Signal cache' tid')
  where reaction cache' val = atomically $ modifyTVar cache' (const val)
{-# INLINABLE newSignal #-}

cancel :: Signal a -> IO ()
cancel (Signal _ tid') = killThread tid'
{-# INLINABLE cancel #-}

-- | Generate events when a cell gets changes.
changes :: Signal a -> IO (Event a)
changes (Signal cac _) = do
  let
    go = do
      val <- liftIO . atomically $ readTVar cac
      yield val
      go
  return (E go)
{-# INLINABLE changes #-}
