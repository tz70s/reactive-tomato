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

main :: IO ()
main = defaultMain [benchEventLatency]
