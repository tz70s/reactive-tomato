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
import Control.Monad
import System.IO

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
  hSetBuffering stdout LineBuffering
  evar <- newEVar
  forkIO $ forM_ [1 .. 10] $ \num -> emit num evar
  let sig0 = events evar
  let sig1 = sig0
  let sig2 = sig0
  react (sig1 `merge` sig2) print
