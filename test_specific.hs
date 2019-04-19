#!/usr/local/bin/stack
{- stack
  runghc
  --resolver lts-13.13
  --package process
-}

{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  )
where

import System.Environment
import System.Process

main :: IO ()
main = do
  testFilter <- head <$> getArgs
  let cmd  = "stack"
  -- FIXME - the argument is not correct, but why?
  let args = ["test", "--interleaved-output", "--ta", "\'-p", "\"" <> testFilter <> "\"\'"]
  putStrLn $ "Test specific with test filter : " <> testFilter
  putStrLn $ "Complete stack command : " <> unwords (cmd : args)
  out <- readProcess cmd args ""
  putStrLn out
