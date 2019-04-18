module Reactive.Tomato.EVar
  ( EVar
  , newEVar
  , emit
  , events
  , react
  , cancel
  )
where

import           Pipes                   hiding ( await )
import           Control.Concurrent.STM

import           Reactive.Tomato.Signal

import qualified Pipes.Concurrent              as PC

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
--   -- We can get signal for free now.
--   let sig1 = events evar
--   let sig2 = foldp (+) 0 sig1
--   react sig2 print
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
-- Note that the lifecycle of EVar should be greater than deriving Signal.
-- Once the EVar gets garbage collected, the deriving signal will be terminated as well.
newEVar :: IO (EVar a)
newEVar = do
  (output, input, seal) <- PC.spawn' PC.unbounded
  return $ EVar (output, input, seal)

-- | Emit value to EVar, typically in a callback function.
emit :: a -> EVar a -> IO ()
emit val (EVar (output, _, _)) = runEffect $ yield val >-> PC.toOutput output

-- | Convert EVar to Signal.
--
-- Problem: if output is terminated, the input will be terminated as well.
events :: MonadIO m => EVar a -> Signal m a
events (EVar (_, input, _)) = Signal $ PC.fromInput input

-- | React Signal to perform side effects.
react :: MonadIO m => Signal m a -> (a -> IO ()) -> m ()
react (Signal p) f = runEffect $ for p $ \v -> liftIO $ f v

-- | Explicitly cancel EVar.
cancel :: EVar a -> IO ()
cancel (EVar (_, _, seal)) = atomically seal
