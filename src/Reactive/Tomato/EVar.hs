module Reactive.Tomato.EVar
  ( EVar
  , newEVar
  , emit
  , events
  , react
  )
where

import Control.Concurrent.STM
import Pipes hiding (await)

import Reactive.Tomato.Event

import qualified Pipes.Concurrent as PC

-- | An event var for composition from callbacks.
-- In Haskell, we're encouraged to use existing green thread system.
-- Hence, we don't actually need to build reactive programming in the style like Rx,
-- which comes along with scheduler. (Rx = /asynchronous dataflow/ + /coroutines/)
-- 
-- EVar compose different haskell threads into signal network.
-- 
-- == Example
-- 
-- @
-- main = do
--   evar <- newEVar
--   forM [1..5] $ \num -> forkIO $ emit num evar
--   -- We can get event for free now.
--   let e1 = events evar
--   react e1 print
-- @
--
-- == Notice
--
-- Currently, the EVar should be carefully maintained lifecycle to be avoid garbage collected,
-- due to we rely on pipes-concurrency. 
-- Otherwise, the events signal may be terminated.
-- 
-- See 'Timer' implementation as reference.
--
-- We should lift this restriction in the future.
newtype EVar a = EVar (PC.Output a, PC.Input a, STM ())

-- | Create a new EVar.
-- 
-- Note that the lifecycle of EVar should be greater than deriving Event.
-- Once the EVar gets garbage collected, the deriving signal will be terminated as well.
newEVar :: IO (EVar a)
newEVar = do
  (output, input, seal) <- PC.spawn' PC.unbounded
  return $ EVar (output, input, seal)
{-# INLINABLE newEVar #-}

-- | Emit value to EVar, typically in a callback function.
emit :: EVar a -> a -> IO ()
emit (EVar (output, _, _)) val = runEffect $ yield val >-> PC.toOutput output
{-# INLINABLE emit #-}

-- | Convert EVar to Event.
--
-- Problem: if output is terminated, the input will be terminated as well.
--
events :: EVar a -> Event a
events (EVar (_, input, _)) = E $ PC.fromInput input
{-# INLINABLE events #-}

-- | React Event to perform side effects.
react :: Event a -> (a -> IO ()) -> IO ()
react (E es) f = runEffect $ for es $ \v -> liftIO $ f v
{-# INLINABLE react #-}
