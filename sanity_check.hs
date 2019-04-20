#!/usr/local/bin/stack
{- stack
  runghc
  --resolver lts-13.13
  --package async
  --package reactive-tomato
-}

{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  )
where

import Control.Concurrent.Async as Async
import Control.Monad

import Reactive.Tomato as RT

main :: IO ()
main = checkSharing

checkRemote :: IO ()
checkRemote = do
  putStrLn "Sanity check for observing remote behavior"
  timer <- every $ milli 10
  let cnt0 = throttle timer $ foldp (+) 0 $ constant (1 :: Int)
  runCluster defaultLocalPubSub $ do
    sidcnt <- sid "cnt0"
    RT.async $ spawn sidcnt cnt0
    cntr <- remote (sidcnt :: Sid Int)
    react cntr print

checkSharing :: IO ()
checkSharing = do
  evar <- newEVar
  a    <- Async.async $ forM_ [1 .. 10] $ \n -> emit n evar
  let init = listGen [1 .. 100]
  let sig0 = init
  let sig1 = init
  let sig2 = sig0 `merge` sig1
  react sig2 print
  wait a
