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

import           Reactive.Tomato

main :: IO ()
main = do
  putStrLn "Sanity check for observing remote behavior"
  timer <- every $ milli 10
  let cnt0 = throttle timer $ foldp (+) 0 $ constant (1 :: Int)
  runCluster defaultLocalPubSub $ do
    sidcnt <- sid "cnt0"
    fork $ spawn sidcnt cnt0
    cntr <- remote (sidcnt :: Sid Int)
    react cntr print
