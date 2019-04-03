module Reactive.Tomato.Test
  ()
where

import           Reactive.Tomato.Signal
import           Reactive.Tomato.Async
import           Pipes
import qualified Pipes.Concurrent              as PC
import qualified Pipes.Prelude                 as P
import           Control.Concurrent             ( forkIO
                                                , threadDelay
                                                )
import           Control.Monad                  ( forever
                                                , forM_
                                                )

{-
  Examples: expectation - unwrap mtl stack into callback.
-}

-- | Example of listener construction.
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

-- | Example3: use Signal to eliminate callback.

{-
run3 :: IO ()
run3 = do
  (output, input) <- PC.spawn PC.unbounded
  reactor 5 $ emit output once
  -- This will be bounded waiting until something emit
  reactC input $ forever $ await >>= lift . print . (+ 5)
-}

-- | Example4: use async to cross heavy computation.

{-
run4 :: IO ()
run4 = do
  (output, input) <- PC.spawn PC.unbounded
  reactor 5 $ emit output $ \num -> async $ once num
  reactC input $ forever $ await >>= lift . print . (+ 5)
-}
