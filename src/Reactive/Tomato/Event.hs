{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}

module Reactive.Tomato.Event
  ( EventT(..)
  , spawn
  , once
  , constE
  )
where

import           Pipes
import qualified Pipes.Concurrent              as PC
import qualified Pipes.Prelude                 as P
import           Control.Monad                  ( forever )
import           Control.Monad.State.Class
import           Control.Monad.Error.Class

-- | EventT is transformer that can produce value across threads.
newtype EventT m a = EventT { unEventT :: Producer a m () }

instance (Monad m) => Functor (EventT m) where
  fmap f (EventT pd) = EventT $ pd >-> P.map f

instance (Monad m) => Applicative (EventT m) where
  pure  = constE
  (<*>) = _ap

instance (Monad m) => Monad (EventT m) where
  return = pure
  (>>=)  = _bind

instance MonadTrans EventT where
  lift m = EventT $ do
    a <- lift m
    yield a

instance (MonadIO m) => MonadIO (EventT m) where
  liftIO m = EventT $ do
    a <- liftIO m
    yield a

instance MonadState s m => MonadState s (EventT m) where
  get   = lift get
  put   = lift . put
  state = lift . state

instance MonadError e m => MonadError e (EventT m) where
  throwError = lift . throwError
  catchError (EventT p0) f = EventT $ catchError p0 $ fmap unEventT f

-- TODO: revision ap and monad instance, currently it's the same as Signal (a.k.a synchronous update.)
-- While event is not like this.

_ap :: Monad m => EventT m (a -> b) -> EventT m a -> EventT m b
_ap (EventT fs) (EventT xs) = EventT $ go fs xs
 where
  go _fs _xs = do
    fe <- lift $ next _fs
    xe <- lift $ next _xs
    case (fe, xe) of
      (Left _        , _             ) -> pure ()
      (_             , Left _        ) -> pure ()
      (Right (f, fs'), Right (x, xs')) -> yield (f x) >> go fs' xs'

_bind :: Monad m => EventT m a -> (a -> EventT m b) -> EventT m b
_bind (EventT xs) f = EventT $ do
  xe <- lift $ next xs
  case xe of
    Left  _        -> pure ()
    Right (x, xs') -> do
      nextEle <- lift . next . unEventT $ f x
      case nextEle of
        Left  _      -> return ()
        Right (n, _) -> do
          yield n
          unEventT $ EventT xs' >>= f

-- | Spawn pair of handlers for communication.
-- 
-- @@
-- (emit, react) <- spawn PC.unbounded
-- reactor 5 $ emit (\num -> EventT (yield num))
-- react print
-- @@
spawn
  :: (MonadIO m', MonadIO m)
  => PC.Buffer b
  -> m' ((a -> EventT m b) -> a -> m (), Consumer b m' () -> m' ())
spawn buffer = do
  (output, input) <- liftIO $ PC.spawn buffer
  return (_emit output, _react input)

_emit :: (MonadIO m) => PC.Output b -> (a -> EventT m b) -> a -> m ()
_emit out emitter = wraps . emitter
 where
  wraps evt = do
    runEffect $ unEventT evt >-> PC.toOutput out
    liftIO PC.performGC

_react :: (MonadIO m) => PC.Input a -> Consumer a m () -> m ()
_react input consumer = do
  runEffect $ PC.fromInput input >-> consumer
  liftIO PC.performGC

once :: (Monad m) => a -> EventT m a
once a = EventT $ yield a

constE :: (Monad m) => a -> EventT m a
constE a = EventT $ forever $ yield a
