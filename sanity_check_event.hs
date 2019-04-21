#!/usr/local/bin/stack
{- stack
  runghc
  --resolver lts-13.13
  --package reactive-tomato
  --package async
-}

module Main
  ( main
  )
where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import System.IO

import Reactive.Tomato.Event as RT

main :: IO ()
main = checkEvent

checkEvent :: IO ()
checkEvent = do
  hSetBuffering stdout LineBuffering
  let e1 = generate [1 .. 10]
  cell <- newCell 0 e1
  let e2 = changes cell
  let e3 = changes cell
  let e4 = e2 `union` e3
  race_ (react e4 print) (threadDelay 3000)
