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
import Control.Monad (forM_)
import Pipes

import Reactive.Tomato.EVar
import Reactive.Tomato.Event

-- | Now idea to close the thread id properly.
data Signal a = Signal { cache :: TVar a, latches :: TVar [EVar a], tid :: ThreadId }

-- Thought: we can react to pump, change tha cache value and propagate to latches.
-- Maybe we can return IO (Signal a), fork a thread and use it to generate events.
-- ThreadId will be cached then we can kill it when we need?
newSignal :: a -> Event a -> IO (Signal a)
newSignal initial pump' = do
  cache' <- newTVarIO initial
  latch' <- newTVarIO []
  tid'   <- forkIO $ react pump' (reaction cache' latch')
  return (Signal cache' latch' tid')
 where
  reaction cache' latch' val = do
    xs <- atomically $ modifyTVar cache' (const val) >> readTVar latch'
    forM_ xs $ \x -> emit x val

cancel :: Signal a -> IO ()
cancel (Signal _ _ tid') = killThread tid'

-- | Generate events when a cell gets changes.
changes :: Signal a -> Event a
changes (Signal _ ls _) = E $ do
  evar <- liftIO newEVar
  liftIO $ atomically $ modifyTVar ls (evar :)
  unE $ events evar
