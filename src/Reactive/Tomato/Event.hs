module Reactive.Tomato.Event
  ( EventT(..)
  , Emit
  , emit
  , react
  , reactC
  , once
  , constE
  )
where

import           Pipes
import           Control.Monad                  ( forever
                                                , forM_
                                                )
import qualified Pipes.Concurrent              as PC
import           Control.Concurrent             ( forkIO
                                                , threadDelay
                                                )

-- | EventT is transformer that can produce value across threads.
newtype EventT m a = EventT { unEventT :: Producer a m () }

type Emit m a = a -> EventT m a

-- TODO: instances

emit :: (MonadIO m) => PC.Output a -> Emit m a -> a -> m ()
emit out emitter = wraps . emitter
 where
  wraps evt = do
    runEffect $ unEventT evt >-> PC.toOutput out
    liftIO PC.performGC

react :: (MonadIO m) => PC.Input a -> (a -> m ()) -> m ()
react input callback = do
  runEffect $ for (PC.fromInput input) $ \v -> lift $ callback v
  liftIO PC.performGC

reactC :: (MonadIO m) => PC.Input a -> Consumer a m () -> m ()
reactC input consumer = do
  runEffect $ PC.fromInput input >-> consumer
  liftIO PC.performGC

once :: (Monad m) => Emit m a
once a = EventT $ yield a

constE :: (Monad m) => Emit m a
constE a = EventT $ forever $ yield a

-- Applying the mapping function into separate threads
async :: (MonadIO m) => EventT m a -> (a -> b) -> EventT m b
async (EventT et) = undefined

forkE :: (MonadIO m, Monad m) => EventT m a -> m (EventT m a)
forkE (EventT et) = do
  (out, input) <- liftIO $ PC.spawn PC.unbounded
  liftIO $ forkIO $ do
    let eff = et >-> PC.toOutput out
    PC.performGC
  return (EventT $ PC.fromInput input)

-- | Examples: expectation - unwrap mtl stack into callback.
reactor :: Int -> (Int -> IO ()) -> IO ()
reactor num callback = forM_ [1 .. 10] $ \nt -> forkIO $ threadDelay 1000000 >> callback (num + nt)

run1 :: IO ()
run1 = reactor 5 (\num -> print (num + 5))

-- | Example2: use pipes concurrency.
run2 :: IO ()
run2 = do
  (output, input) <- PC.spawn PC.unbounded
  reactor 5 $ \num -> do
    runEffect $ yield num >-> PC.toOutput output
    PC.performGC
  runEffect $ for (PC.fromInput input) $ \num -> lift $ print (num + 5)

-- | Example3: use EventT and Emit to encapsulate concurrency.

run3 :: IO ()
run3 = do
  (output, input) <- PC.spawn PC.unbounded
  reactor 5 $ emit output once
  -- This will be bounded waiting until something emit
  reactC input $ forever $ await >>= lift . print . (+ 5)
