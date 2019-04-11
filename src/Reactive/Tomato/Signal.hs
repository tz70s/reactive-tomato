{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}

module Reactive.Tomato.Signal
  ( Signal(..)
  , constant
  , listGen
  , interpret
  , merge
  , mergeAll
  , filterp
  , foldp
  )
where

import           Pipes
import qualified Pipes.Prelude                 as PP
import           Control.Monad                  ( forever )
import           Control.Monad.State.Class
import           Control.Monad.Error.Class
import           Control.Monad.Identity
import           Control.Applicative
import           Data.Foldable

-- | Signal abstraction - the representation is isomorphic to latest value in a stream.
newtype Signal m a = Signal { unS :: Producer a m () }

-- | Create a constant signal which is isomorphic to a infinite list of constant value.
--
-- @
-- let sig1 = constant 5
-- let first10 = take 10 $ interpret sig1
-- first10 == [5, 5, 5, 5, 5, 5, 5, 5, 5, 5]
-- @
constant :: (Monad m) => a -> Signal m a
constant = _pure

-- | Create a siganl from a list, which can be finite or infinite.
--
-- @
-- let sig1 = listGen [1, 2, 3, 4, 5]
-- interpret sig1 == [1, 2, 3, 4, 5]
-- @
listGen :: Monad m => [a] -> Signal m a
listGen = Signal . go where go = foldr ((>>) . yield) mempty

-- | Interpret pure signal into list, useful to inspecting signal transformation.
interpret :: Signal Identity a -> [a]
interpret = PP.toList . unS

instance Monad m => Functor (Signal m) where
  fmap f (Signal as) = Signal $ as >-> PP.map f

instance Monad m => Applicative (Signal m) where
  pure  = _pure
  (<*>) = _ap

instance Monad m => Alternative (Signal m) where
  empty = _empty
  (<|>) = _choice

instance Monad m => Monad (Signal m) where
  return = _pure
  (>>=)  = _bind

instance MonadTrans Signal where
  lift m = Signal $ do
    a <- lift m
    yield a

instance (MonadIO m) => MonadIO (Signal m) where
  liftIO m = Signal $ do
    a <- liftIO m
    yield a

instance MonadState s m => MonadState s (Signal m) where
  get   = lift get
  put   = lift . put
  state = lift . state

instance MonadError e m => MonadError e (Signal m) where
  throwError = lift . throwError
  catchError (Signal p0) f = Signal $ catchError p0 $ fmap unS f

_pure :: Monad m => a -> Signal m a
_pure = Signal . forever . yield

_empty :: Monad m => Signal m a
_empty = Signal $ return ()

_ap :: Monad m => Signal m (a -> b) -> Signal m a -> Signal m b
_ap (Signal fs) (Signal xs) = Signal $ go fs xs
 where
  go _fs _xs = do
    fe <- lift $ next _fs
    xe <- lift $ next _xs
    case (fe, xe) of
      (Left _        , _             ) -> pure ()
      (_             , Left _        ) -> pure ()
      (Right (f, fs'), Right (x, xs')) -> yield (f x) >> go fs' xs'

-- FIXME: need a way to investigate this is a correct implementation.
_choice :: Monad m => Signal m a -> Signal m a -> Signal m a
_choice (Signal as) (Signal bs) = Signal $ go as bs
 where
  go _as _bs = do
    ae <- lift $ next _as
    case ae of
      Left _ -> do
        be <- lift $ next _bs
        case be of
          Left  _        -> pure ()
          -- Reverse the arguments for fairness.
          Right (b, be') -> yield b >> go be' _as
      Right (a, as') -> yield a >> go _bs as'

_bind :: Monad m => Signal m a -> (a -> Signal m b) -> Signal m b
_bind (Signal xs) f = Signal $ do
  xe <- lift $ next xs
  case xe of
    Left  _        -> pure ()
    Right (x, xs') -> do
      _next <- lift . next . unS $ f x
      case _next of
        Left  _      -> return ()
        Right (n, _) -> do
          yield n
          unS $ Signal xs' >>= f

-- | Merge two signal by interleaving event occurrences.
-- 
-- Comparing to merging in applicative instance, i.e. @liftA2 (+) (constant 1) (constant 2)@,
-- the events are propagated \asynchronously\.
merge :: Monad m => Signal m a -> Signal m a -> Signal m a
merge = _choice

-- FIXME: this isn't balance at all, should we pursuing fairness?
mergeAll :: (Foldable t, Monad m) => t (Signal m a) -> Signal m a
mergeAll = asum

-- | Filter elements with predicate function.
--
-- @
-- let cnt = foldp (+) 0 $ constant 1
-- let even = filterp (\i -> i `mod` 2 == 0) cnt
-- interpret even == [2, 4 ..]
-- @
filterp :: Monad m => (a -> Bool) -> Signal m a -> Signal m a
filterp f (Signal p) = Signal $ p >-> PP.filter f

-- | Past dependent folding.
--
-- @
-- let counter = foldp (+) 0 $ constant 1
-- interpret counter == [1..]
-- @
foldp :: Monad m => (a -> s -> s) -> s -> Signal m a -> Signal m s
foldp f s0 (Signal from) = Signal $ from >-> go s0
 where
  go _state = do
    v <- await
    let newState = f v _state
    yield newState
    go newState
