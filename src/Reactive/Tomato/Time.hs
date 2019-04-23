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

import Control.Applicative
import Control.Concurrent hiding (yield)
import Control.Concurrent.Async
import Control.Monad.IO.Class
import Pipes hiding (every)

import Reactive.Tomato.EVar
import Reactive.Tomato.Event

import qualified Pipes.Concurrent as PC

-- | Necessary to keep EVar because of garbage collection.
newtype Timer = T (Event (), EVar ())

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
every :: Time -> IO Timer
every intvl = do
  evar <- newEVar
  _    <- async $ go evar
  return $ T (events evar, evar)
 where
  toMicro (Second t) = t * 1000000
  toMicro (Milli  t) = t * 1000
  toMicro (Micro  t) = t

  go evar = do
    threadDelay $ toMicro intvl
    emit evar ()
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
start :: Timer -> Event ()
start (T (sig, _)) = sig
{-# INLINABLE start #-}

-- | Throttle signal propagation speed in milliseconds.
--
-- @
-- let sig = throttle (second 1) $ constant 1
-- @
throttle :: Timer -> Event a -> Event a
throttle timer = liftA2 (flip const) (start timer)
{-# INLINABLE throttle #-}

-- | Snapshot the value of specific time point.
-- 
-- @
-- let counter = foldp (+) 0 $ constant 1
-- -- tick every one second
-- let tick = every $ second 1
-- let snap = snapshot tick counter
-- @
snapshot :: Timer -> Event a -> Event a
snapshot timer (E as) = E $ do
  res <- lift $ next as
  case res of
    Left  _      -> pure ()
    Right (x, _) -> do
      (output, input) <- liftIO $ PC.spawn $ PC.latest x
      a               <- lift . async $ runEffect $ as >-> PC.toOutput output
      unE $ liftA2 const (E (PC.fromInput input)) (start timer)
      liftIO $ wait a

-- | Window the value of specific time window (interval).
--
-- FIXME - the implementation is not correct.
window :: Timer -> Event a -> Event (Event a)
window timer (E as) = subroutine <$ start timer
 where
  subroutine = E $ do
    (output, input) <- liftIO $ PC.spawn PC.unbounded
    a               <- lift . async $ runEffect $ as >-> PC.toOutput output
    PC.fromInput input
    liftIO $ wait a
