module Reactive.Tomato.Signal.Tests
  ( tests
  )
where

import Test.Tasty
import Test.Tasty.HUnit

import Reactive.Tomato as RT

tests :: TestTree
tests = testGroup "Signal Tests" [testCase "Changes to propagate events" testChanges]

testChanges :: Assertion
testChanges = do
  let e0 = RT.repeat (1 :: Int)
  sig <- newSignal 1 e0
  e1  <- changes sig
  xs  <- interpret (RT.take 10 e1)
  xs @?= replicate 10 1
