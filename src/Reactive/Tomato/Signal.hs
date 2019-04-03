{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}

module Reactive.Tomato.Signal
  ( Signal(..)
  , interpret
  , constant
  )
where

import           Pipes
import           Pipes.Prelude                 as PP
import           Control.Monad                  ( forever )
import           Control.Monad.State.Class
import           Control.Monad.Error.Class
import           Control.Monad.Identity
import           Control.Applicative

-- | S abstraction - the representation is isomorphic to latest value in a stream.
newtype Signal m a = Signal { unS :: Producer a m () }

constant :: (Monad m) => a -> Signal m a
constant = _pure

-- | Interpret pure signal into list, useful to inspecting signal transformation.
interpret :: Signal Identity a -> [a]
interpret = PP.toList . unS

-- | Functor instance, concatenate a mapping pipes to fmap it.
instance Monad m => Functor (Signal m) where
  fmap f (Signal as) = Signal $ as >-> PP.map f

instance Monad m => Applicative (Signal m) where
  pure  = _pure
  (<*>) = _ap

instance Monad m => Alternative (Signal m) where
  empty = _empty
  (<|>) (Signal as) (Signal bs) = undefined

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

-- | We need a way to peek the value from producer.
_choice :: Monad m => Signal m a -> Signal m a -> Signal m a
_choice (Signal as) (Signal bs) = Signal $ go as bs where go _as _bs = undefined

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
