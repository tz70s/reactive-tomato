module Reactive.Tomato.EVar
  ( EVar
  )
where

-- | An event var for composition from callbacks.
-- In Haskell, we're encouraged to use existing green thread system.
-- Hence, we don't actually need to build reactive programming in the style like Rx,
-- which comes along with scheduler. (Rx = /asynchronous dataflow/ + /coroutines/)
-- 
-- The Evar compose different haskell threads into signal network.
-- In theory, this is just a publish/subscribe pattern.
-- A similar construction of this type can be referenced to <https://github.com/transient-haskell/transient transient>
data EVar

-- | Spawn pair of handlers for communication.
-- 
-- @
-- (emit, react) <- spawn PC.unbounded
-- reactor 5 $ emit (\num -> EventT (yield num))
-- react print
-- @

{-

Brain storming : how can we use EVar to abstract this?

spawn
  :: (MonadIO m', MonadIO m)
  => PC.Buffer b
  -> m' ((a -> EventT m b) -> a -> m (), Consumer b m' () -> m' ())
spawn buffer = do
  (output, input) <- liftIO $ PC.spawn buffer
  return (_emit output, _react input)

_emit :: (MonadIO m) => PC.Output b -> (a -> EventT m b) -> a -> m ()
_emit out emitter = wraps . emitter
 where
  wraps evt = do
    runEffect $ unEventT evt >-> PC.toOutput out
    liftIO PC.performGC

_react :: (MonadIO m) => PC.Input a -> Consumer a m () -> m ()
_react input consumer = do
  runEffect $ PC.fromInput input >-> consumer
  liftIO PC.performGC

-}
