{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  )
where

import Criterion.Main

import Reactive.Tomato as RT
import Reactive.Tomato.Remote
import Reactive.Tomato.Time

latencyRemoteEvent :: Event Int -> Int -> IO [Int]
latencyRemoteEvent e num = interpret $ RT.take num e

prepareEvent :: IO (Event Int)
prepareEvent = do
  let cnt = foldp (+) 0 (RT.repeat (1 :: Int))
  runCluster (Broker "127.0.0.1" 6379) $ do
    sid0 <- sid "cnt0"
    _    <- spawn sid0 cnt
    filterJust <$> remote sid0

prepareSignal :: IO (Event Int)
prepareSignal = do
  let cnt = foldp (+) 0 (RT.repeat (1 :: Int))
  signal <- runCluster (Broker "127.0.0.1" 6379) $ do
    sid0 <- sid "cnt0"
    _    <- spawn sid0 cnt
    remote sid0
  events <- changes signal
  return (filterJust events)

benchEventLatency :: Event Int -> Benchmark
benchEventLatency evt = bgroup
  "Remote events latency"
  [ bench "latency-1-remote" $ nfIO (latencyRemoteEvent evt 1)
  , bench "latency-10-remote" $ nfIO (latencyRemoteEvent evt 10)
  , bench "latency-100-remote" $ nfIO (latencyRemoteEvent evt 100)
  , bench "latency-1000-remote" $ nfIO (latencyRemoteEvent evt 1000)
  , bench "latency-10000-remote" $ nfIO (latencyRemoteEvent evt 10000)
  ]

benchSignalLatency :: Event Int -> Benchmark
benchSignalLatency evt = bgroup
  "Remote signal latency"
  [ bench "latency-1-remote" $ nfIO (latencyRemoteEvent evt 1)
  , bench "latency-10-remote" $ nfIO (latencyRemoteEvent evt 10)
  , bench "latency-100-remote" $ nfIO (latencyRemoteEvent evt 100)
  , bench "latency-1000-remote" $ nfIO (latencyRemoteEvent evt 1000)
  , bench "latency-10000-remote" $ nfIO (latencyRemoteEvent evt 10000)
  ]

main :: IO ()
main = do
  evt    <- prepareEvent
  evtSig <- prepareSignal
  defaultMain [benchEventLatency evt, benchSignalLatency evtSig]
