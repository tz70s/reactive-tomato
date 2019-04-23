module Main
  ( main
  )
where

import Criterion.Main

import Reactive.Tomato as RT

latencyEventFoldp :: Int -> IO [Int]
latencyEventFoldp num = do
  let counter = foldp (+) 0 (RT.repeat 1)
  interpret (RT.take num counter)

benchEventLatency :: Benchmark
benchEventLatency = bgroup
  "Event processing latency"
  [ bench "latency-1-event" $ nfIO (latencyEventFoldp 1)
  , bench "latency-10-event" $ nfIO (latencyEventFoldp 10)
  , bench "latency-100-event" $ nfIO (latencyEventFoldp 100)
  , bench "latency-1000-event" $ nfIO (latencyEventFoldp 1000)
  , bench "latency-10000-event" $ nfIO (latencyEventFoldp 10000)
  ]

latencyEventFoldpUnion :: Int -> IO [Int]
latencyEventFoldpUnion num = do
  let counter = foldp (+) 0 (RT.repeat 1)
  interpret (RT.take num (counter `union` counter))

benchEventUnion :: Benchmark
benchEventUnion = bgroup
  "Event processing latency with union"
  [ bench "latency-1-event-union" $ nfIO (latencyEventFoldpUnion 1)
  , bench "latency-10-event-union" $ nfIO (latencyEventFoldpUnion 10)
  , bench "latency-100-event-union" $ nfIO (latencyEventFoldpUnion 100)
  , bench "latency-1000-event-union" $ nfIO (latencyEventFoldpUnion 1000)
  , bench "latency-10000-event-union" $ nfIO (latencyEventFoldpUnion 10000)
  ]

latencyEventFoldpAp :: Int -> IO [Int]
latencyEventFoldpAp num = do
  let counter = foldp (+) 0 (RT.repeat 1)
  interpret (RT.take num (RT.repeat (+ 1) <*> counter))

benchEventAp :: Benchmark
benchEventAp = bgroup
  "Event processing latency with applicative"
  [ bench "latency-1-event-ap" $ nfIO (latencyEventFoldpAp 1)
  , bench "latency-10-event-ap" $ nfIO (latencyEventFoldpAp 10)
  , bench "latency-100-event-ap" $ nfIO (latencyEventFoldpAp 100)
  , bench "latency-1000-event-ap" $ nfIO (latencyEventFoldpAp 1000)
  , bench "latency-10000-event-ap" $ nfIO (latencyEventFoldpAp 10000)
  ]

main :: IO ()
main = defaultMain [benchEventLatency, benchEventUnion, benchEventAp]
