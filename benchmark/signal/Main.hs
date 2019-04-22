module Main
  ( main
  )
where

import Criterion.Main

import Reactive.Tomato as RT
import Reactive.Tomato.Time

latencySignal :: Event Int -> Int -> IO [Int]
latencySignal evt num = interpret $ RT.take num evt

prepareSignal :: IO (Signal Int)
prepareSignal = do
  let counter = foldp (+) 0 (RT.repeat 1)
  newSignal (-1) counter

benchSignalLatency :: Event Int -> Benchmark
benchSignalLatency evt = bgroup
  "Signal read+write latency"
  [ bench "latency-1-with-signal-rw" $ nfIO (latencySignal evt 1)
  , bench "latency-10-with-signal-rw" $ nfIO (latencySignal evt 10)
  , bench "latency-100-with-signal-rw" $ nfIO (latencySignal evt 100)
  , bench "latency-1000-with-signal-rw" $ nfIO (latencySignal evt 1000)
  , bench "latency-10000-with-signal-rw" $ nfIO (latencySignal evt 10000)
  ]

main :: IO ()
main = do
  sig0 <- prepareSignal
  evt  <- changes sig0
  defaultMain [benchSignalLatency evt]
