{-# LANGUAGE ScopedTypeVariables #-}

module Reactive.Tomato.Signal
  ( Signal(..)
  , signal
  , sample
  , changes
  )
where

import Control.Concurrent.Async hiding (cancel)
import Control.Monad (forever, when)

import Pipes
import Reactive.Tomato.Event

import qualified Pipes.Prelude as PP
import qualified Pipes.Concurrent as PC

-- | Now idea to close the thread id properly.
data Signal a = S { initial :: a, unS :: Producer a IO () }

signal :: a -> Event a -> IO (Signal a)
signal a (E es) = do
  (output, input) <- PC.spawn $ PC.latest a
  _               <- async $ runEffect (es >-> PC.toOutput output)
  return (S a $ PC.fromInput input)

sample :: Signal a -> Event (a -> b) -> Event b
sample (S _ ss) ef = ef <*> E ss

changes :: Eq a => Signal a -> Event a
changes (S val ss) = E (ss >-> check val)
 where
  check curr = do
    val <- await
    if val == curr then check curr else yield val >> check val

instance Functor Signal where
  fmap f (S s0 ss) = S (f s0) $ ss >-> PP.map f

instance Applicative Signal where
  pure a = S a (forever $ yield a)

  (S f0 fs) <*> (S s0 ss) = S (f0 s0) $ go fs ss
   where
    go _fs _xs = do
      fe <- lift $ next _fs
      xe <- lift $ next _xs
      case (fe, xe) of
        (Left _        , _             ) -> pure ()
        (_             , Left _        ) -> pure ()
        (Right (f, fs'), Right (x, xs')) -> yield (f x) >> go fs' xs'
