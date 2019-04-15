{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}

module Reactive.Tomato.Signal
  ( Signal(..)
  , constant
  , listGen
  , filter
  , filterJust
  , foldp
  , interpret
  , interpretM
  , take
  )
where

import           Prelude                       hiding (filter, take)
import           Pipes
import qualified Pipes.Prelude                 as PP
import           Control.Monad                  ( forever )
import           Control.Monad.State.Class
import           Control.Monad.Error.Class
import           Control.Monad.Identity
import           Control.Applicative

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
          -- TODO - possible optimize? We already know that _as is terminated, then we can reduce the calling it?
          Right (b, be') -> yield b >> go _as be'
      Right (a, as') -> yield a >> go as' _bs

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

-- | Filter elements with predicate function.
--
-- @
-- let cnt = foldp (+) 0 $ constant 1
-- let even = filter (\i -> i `mod` 2 == 0) cnt
-- interpret even == [2, 4 ..]
-- @
filter :: Monad m => (a -> Bool) -> Signal m a -> Signal m a
filter f (Signal p) = Signal $ p >-> PP.filter f

-- | Extract maybe elements into only just value.
--
-- @
-- let sig0 = listGen [Just 1, Just 2, Nothing, Just 3, Nothing]
-- let sig1 = filterJust sig0
-- interpret sig1 == [1, 2, 3]
-- @
filterJust :: Monad m => Signal m (Maybe a) -> Signal m a
filterJust (Signal p) = Signal $ p >-> extract
  where
    extract = do
      v <- await
      case v of
        Just x -> yield x >> extract
        Nothing -> extract

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

-- | Interpret pure signal into list, useful to inspecting signal transformation.
interpret :: Signal Identity a -> [a]
interpret = PP.toList . unS

interpretM :: Monad m => Signal m a -> m [a]
interpretM (Signal p) = PP.toListM p

take :: Monad m => Int -> Signal m a -> Signal m a
take times (Signal p) = Signal $ p >-> PP.take times
