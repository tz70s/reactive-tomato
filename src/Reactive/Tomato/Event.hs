{-# LANGUAGE ScopedTypeVariables #-}

module Reactive.Tomato.Event
  ( Event(..)
  , filter
  , filterJust
  , foldp
  , union
  , unionAll
  , last
  , generate
  , Reactive.Tomato.Event.repeat
  , interpret
  , take
  )
where

import Control.Applicative
import Control.Concurrent.Async hiding (cancel)
import Control.Monad (forM)
import Pipes
import Prelude hiding (filter, take, last)

import qualified Pipes.Concurrent as PC
import qualified Pipes.Prelude as PP

-- Design Note:
-- 1. Event can't be shared (recomputation), all events should be generated via EVars from callbacks.
-- 2. Stateful unit is cached in Signal, which has much limit interface, but similar to time-varying value.

-- | Event abstraction.
--
-- Event can be combined with various way (i.e. functor, applicative and monad).
-- 
-- Note on Applicative instance: merging events are synchronous,
-- for asynchronous merging, please refer to 'union' or 'unionAll' variants.
--
-- Please refer to unit/property tests for behavior of events.
newtype Event a = E { unE :: Producer a IO () }

instance Functor Event where
  fmap f (E es) = E $ es >-> PP.map f

instance Applicative Event where
  pure e = E $ yield e

  (E ef) <*> (E es) = E $ go ef es
   where
    go _fs _xs = do
      fe <- lift $ next _fs
      xe <- lift $ next _xs
      case (fe, xe) of
        (Left _        , _             ) -> pure ()
        (_             , Left _        ) -> pure ()
        (Right (f, fs'), Right (x, xs')) -> yield (f x) >> go fs' xs'

instance Monad Event where
  return = pure

  (E xs) >>= f = E $ do
    xe <- lift $ next xs
    case xe of
      Left  _        -> pure ()
      Right (x, xs') -> do
        _next <- lift . next . unE $ f x
        case _next of
          Left  _      -> return ()
          Right (n, _) -> do
            yield n
            unE $ E xs' >>= f

instance Alternative Event where
  empty = E $ return ()

  (E es0) <|> (E es1) = E $ go es0 es1
   where
    go as bs = do
      ae <- lift $ next as
      case ae of
        Left _ -> do
          be <- lift $ next bs
          case be of
            Left  _        -> pure ()
            -- TODO - possible optimize? We already know that as is terminated, then we can reduce the calling it?
            Right (b, be') -> yield b >> go as be'
        Right (a, as') -> yield a >> go as' bs

-- | Infinite sequence of event occurrences.
repeat :: a -> Event a
repeat = generate . Prelude.repeat
{-# INLINABLE repeat #-}

-- | Generate events from a list, which can be finite or infinite.
-- 
-- Note that this is not intended to be used.
-- In general, you should almost work with EVar emission.
--
-- @
-- let e1 = generate [1, 2, 3, 4, 5]
-- xs <- interpret e1
-- xs == [1, 2, 3, 4, 5]
-- @
generate :: [a] -> Event a
generate = E . go where go = foldr ((>>) . yield) mempty
{-# INLINABLE generate #-}

-- | Interpret 'Event' into list within monad context, useful to inspecting events transformation.
--
-- Note that this is intentionally used in testing.
interpret :: Event a -> IO [a]
interpret (E es) = PP.toListM es
{-# INLINABLE interpret #-}

-- | Merge two 'Event' by interleaving event occurrences.
-- 
-- Comparing to merging in applicative instance, i.e. @liftA2 (+) (repeat 1) (repeat 2)@,
-- the events are propagated \asynchronously\.
--
-- i.e.
--
-- @
-- let e0 = foldp (+) 0 $ repeat 1
-- let e1 = foldp (+) 0 $ repeat 2
-- let e2 = union e0 e1
-- -- Ideally, the sequence will be interleaved with each events.
-- -- sigm ~> [1, 2, 2, 3, 4, 4, 6, 5 ..]
-- @
-- 
-- There's no guarantee how the interleaving effect is performed.
-- If you need that guarantee, please consider using FRP library.
-- That is, the interleaving effect is non-determinism,
-- the only guarantee is we preserved the FIFO ordering in each events.
union :: Event a -> Event a -> Event a
union s1 s2 = unionAll [s1, s2]
{-# INLINABLE union #-}

-- | Merge list of events by interleaving event occurrences.
--
-- There's no guarantee how the interleaving effect is performed.
-- If you need that guarantee, please consider using FRP library.
-- That is, the interleaving effect is non-determinism,
-- the only guarantee is we preserved the FIFO ordering in each events.
unionAll :: Traversable t => t (Event a) -> Event a
unionAll xs = E $ do
  (output, input) <- liftIO $ PC.spawn PC.unbounded
  as              <- liftIO $ forM xs $ \(E as) -> async $ do
    runEffect $ as >-> PC.toOutput output
    PC.performGC
  PC.fromInput input
  liftIO $ mapM_ wait as

-- | Take bounded elements from events, then terminate it.
take :: Int -> Event a -> Event a
take times (E es) = E $ es >-> PP.take times
{-# INLINABLE take #-}

-- | Filter elements with predicate function.
--
-- @
-- let cnt = foldp (+) 0 $ repeat 1
-- let even = filter (\i -> i `mod` 2 == 0) cnt
-- interpret even == [2, 4 ..]
-- @
filter :: (a -> Bool) -> Event a -> Event a
filter f (E p) = E $ p >-> PP.filter f
{-# INLINABLE filter #-}

-- | Extract maybe elements into only just value.
--
-- @
-- let e0 = generate [Just 1, Just 2, Nothing, Just 3, Nothing]
-- let e1 = filterJust sig0
-- xs <- interpret e1
-- -- xs == [1, 2, 3]
-- @
filterJust :: Event (Maybe a) -> Event a
filterJust (E p) = E $ p >-> extract
 where
  extract = do
    v <- await
    case v of
      Just x  -> yield x >> extract
      Nothing -> extract
{-# INLINABLE filterJust #-}

-- | Past dependent folding.
--
-- @
-- let counter = foldp (+) 0 $ repeat 1
-- xs <- interpret counter
-- xs == [1..]
-- @
foldp :: (a -> s -> s) -> s -> Event a -> Event s
foldp f s0 (E from) = E $ from >-> go s0
 where
  go _state = do
    v <- await
    let newState = f v _state
    yield newState
    go newState
{-# INLINABLE foldp #-}

-- | Take the last element of events.
-- Useful when you need to reduce elements from window.
--
-- @
-- main = do
--   timer0 <- every 100
--   timerWindow <- every 1000
--   let sig0 = throttle timer0 $ foldp (+) 0 $ repeat 1
--   let latestW = last <$> window timerWindow sig0
--   xs <- interpretM latestW
--   print xs
--   -- xs should be something like [1, 10, 20, 30, ..]
-- @
last :: Event a -> IO a
last (E p) = do
  res <- next p
  case res of
    Left  _           -> empty
    Right (val', ps') -> go ps' val'
 where
  go ps val = do
    res <- next ps
    case res of
      Left  _           -> return val
      Right (val', ps') -> go ps' val'
{-# INLINABLE last #-}
