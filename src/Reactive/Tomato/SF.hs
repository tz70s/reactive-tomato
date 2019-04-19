module Reactive.Tomato.SF
  ( SF(..)
  , arr
  )
where

import Control.Category
import Prelude hiding (id, (.))

import Reactive.Tomato.Signal

newtype SF m a b = SF { unSF :: Signal m a -> Signal m b }

instance Category (SF m) where
  id = SF id
  (SF sf1) . (SF sf2) = SF (sf1 . sf2)

instance Monad m => Functor (SF m a) where
  fmap f (SF sf) = SF $ (fmap . fmap) f sf

instance Monad m => Applicative (SF m a) where
  pure x = SF $ \_ -> constant x
  (SF sff) <*> (SF sf0) = SF (\sig -> sff sig <*> sf0 sig)

-- TODO - support arrow instances?
-- How to implement functions remaining?

arr :: Monad m => (a -> b) -> SF m a b
arr f = SF $ fmap f
