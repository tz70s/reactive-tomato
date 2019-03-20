module Reactive.Tomato.Signal
  ( Signal
  , runSignal
  , constant
  )
where

import           Pipes
import           Pipes.Prelude                 as Pipes
import           Control.Monad                  ( forever )

-- | Signal abstraction - the representation is isomorphic to latest value in a stream.
newtype Signal m a = Signal { unSignal :: Producer a m () }

-- | Run signal into producer and can be operated with pipes operators.
runSignal :: Signal m a -> Producer a m ()
runSignal = unSignal

constant :: (Monad m) => a -> Signal m a
constant = _pure

-- | Functor instance, concatenate a mapping pipes to fmap it.
instance Monad m => Functor (Signal m) where
  fmap f (Signal as) = Signal $ as >-> Pipes.map f

instance Monad m => Applicative (Signal m) where
  pure  = _pure
  (<*>) = _ap

instance Monad m => Monad (Signal m) where
  return = _pure
  (>>=)  = _bind

_pure :: Monad m => a -> Signal m a
_pure = Signal . forever . yield

_ap :: Monad m => Signal m (a -> b) -> Signal m a -> Signal m b
_ap (Signal fs) (Signal xs) = Signal $ go fs xs
 where
  go fs xs = do
    fe <- lift $ Pipes.next fs
    xe <- lift $ Pipes.next xs
    case (fe, xe) of
      (Left _        , _             ) -> pure ()
      (_             , Left _        ) -> pure ()
      (Right (f, fs'), Right (x, xs')) -> yield (f x) >> go fs' xs'

_bind :: Monad m => Signal m a -> (a -> Signal m b) -> Signal m b
_bind (Signal xs) f = Signal $ do
  xe <- lift $ Pipes.next xs
  case xe of
    Left  _        -> pure ()
    Right (x, xs') -> do
      next <- lift . Pipes.next . unSignal $ f x
      case next of
        Left  _      -> return ()
        Right (n, _) -> do
          yield n
          unSignal $ Signal xs' >>= f
