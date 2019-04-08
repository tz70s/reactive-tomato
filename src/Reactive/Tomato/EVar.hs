module Reactive.Tomato.EVar
  ( EVar
  , newEVar
  , emit
  , events
  , react
  )
where

import           Reactive.Tomato.Signal
import           Pipes
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
--   forM [1..5] $ \num -> forkIO $ emit num
--   -- We can get signal for free now.
--   let sig1 = events evar
--   let sig2 = foldp (+) 0 sig1
--   react sig2 print
-- @
newtype EVar a = EVar { unE :: (PC.Input a, PC.Output a) }

newEVar :: IO (EVar a)
newEVar = do
  (output, input) <- PC.spawn PC.unbounded
  return $ EVar (input, output)

emit :: a -> EVar a -> IO ()
emit val (EVar (_, output)) = do
  runEffect $ yield val >-> PC.toOutput output
  -- TODO: this is a full GC, we need to consider remove this.
  PC.performGC

events :: MonadIO m => EVar a -> Signal m a
events (EVar (input, _)) = Signal $ PC.fromInput input

react :: MonadIO m => Signal m a -> (a -> IO ()) -> m ()
react (Signal p) f = runEffect $ for p $ \v -> liftIO $ f v
