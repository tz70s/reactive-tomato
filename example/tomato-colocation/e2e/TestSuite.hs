{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  )
where

import Control.Concurrent.Async
import Control.Monad
import Data.Aeson
import Test.Tasty
import Test.Tasty.HUnit

import Tomato.Colocation

import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Network.WebSockets as WS

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup
  "Colocation integration test"
  [ testCase "Spawn single request"           testSingle
  , testCase "Spawn two concurrent requests"  testConcTwo
  , testCase "Spawn many concurrent requests" testConcMany
  ]

runClientLocal :: WS.ClientApp a -> IO a
runClientLocal = WS.runClient "127.0.0.1" 9160 ""

singleInteract :: WS.WebSocketsData a => BSL.ByteString -> WS.ClientApp a
singleInteract bs conn = do
  WS.sendBinaryData conn bs
  WS.receiveData conn

testSingle :: Assertion
testSingle = do
  let bs0 = encode $ RealWorldEvent (1.0, 1.0) (1.0, 1.0) "test" "test0"
  result0 <- runClientLocal (singleInteract bs0)
  result0 @?= ("[]" :: Text.Text)

testConcTwo :: Assertion
testConcTwo = do
  let bs0 = encode $ RealWorldEvent (1.0, 1.0) (1.0, 1.0) "test" "test0"
  let bs1 = encode $ RealWorldEvent (10.0, 20.0) (1.0, 1.0) "test" "test1"
  let run = runClientLocal . singleInteract
  (result0, result1) <- run bs0 `concurrently` run bs1
  Text.putStrLn result0
  Text.putStrLn result1

testConcMany :: Assertion
testConcMany = do
  let bs num = encode $ RealWorldEvent (num, 1) (1.0, 1.0) "test" ("test-" <> Text.pack (show num))
  let run = runClientLocal . singleInteract
  xs <- forConcurrently [1 .. 50] $ \num -> run (bs num)
  length xs @?= 50
  forM_ xs $ \text -> Text.putStrLn text
