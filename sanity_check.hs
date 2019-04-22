#!/usr/local/bin/stack
{- stack
  runghc
  --resolver lts-13.13
  --package reactive-tomato
-}

{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  )
where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import System.IO

import Reactive.Tomato as RT
import Reactive.Tomato.Time
import Reactive.Tomato.Remote

main :: IO ()
main = checkRemote

checkThrottling :: IO ()
checkThrottling = do
  timer <- every $ milli 100
  let e0 = throttle timer $ foldp (+) 0 (RT.repeat 1)
  let e1 = RT.take 10 e0
  xs <- interpret e1
  print xs

checkRemote :: IO ()
checkRemote = do
  let cnt = foldp (+) 0 (RT.repeat (1 :: Int))
  updateTimer <- every $ milli 10
  let updates = throttle updateTimer cnt
  cnt1 <- runCluster defaultLocalPubSub $ do
    -- This is useful to eliminate explicit sid construction.
    -- If there's a sid which will be reuse,
    -- calling 'remote' will inference the sid phantom type as well as the signal type.
    sid0 <- sid "cnt0"
    -- In general, spawn will block the thread until the signal is terminated.
    -- Therefore, the Cluster monad is instance of MonadFork that you can fork the spawn into separate threads.
    spawn sid0 updates
    remote sid0
  xs <- interpret (RT.take 10 cnt1)
  print xs

{-
checkEvent :: IO ()
checkEvent = do
  hSetBuffering stdout LineBuffering
  let e1 = generate [1 .. 10]
  signal <- newSignal 0 e1
  let e2 = changes signal
  let e3 = changes signal
  let e4 = e2 `union` e3
  race_ (react e4 print) (threadDelay 3000)
-}
