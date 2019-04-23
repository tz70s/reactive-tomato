module Main
  ( main
  )
where

import Control.Concurrent.Async
import Control.Monad (void, forever)
import Criterion.Main

import Reactive.Tomato as RT

emitValues :: EVar Int -> IO ()
emitValues evar = void $ async $ forever $ emit evar 1

flatten :: Int -> Event Int -> IO [Int]
flatten times event = interpret (RT.take times event)

benchEVarLatency :: Event Int -> Benchmark
benchEVarLatency e0 = bgroup
  "EVar emitting latency"
  [ bench "latency-1-evar" $ nfIO (flatten 1 e0)
  , bench "latency-10-evar" $ nfIO (flatten 10 e0)
  , bench "latency-100-evar" $ nfIO (flatten 100 e0)
  , bench "latency-1000-evar" $ nfIO (flatten 1000 e0)
  , bench "latency-10000-evar" $ nfIO (flatten 10000 e0)
  ]

main :: IO ()
main = do
  evar <- newEVar
  let e0 = events evar
  emitValues evar
  defaultMain [benchEVarLatency e0]
