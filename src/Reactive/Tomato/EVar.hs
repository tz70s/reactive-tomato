module Reactive.Tomato.EVar
  ( EVar
  , newEVar
  , emit
  , events
  , react
  , cancel
  , Bref
  , bref
  , BVar
  , newBVar
  , eventsB
  , emitB
  , register
  , fromList
  , derefBVar
  )
where

import Control.Concurrent.STM
import Data.String
import Pipes hiding (await)

import Reactive.Tomato.Signal

import qualified Data.Map as Map
import qualified Pipes.Concurrent as PC

-- | An event var for composition from callbacks.
-- In Haskell, we're encouraged to use existing green thread system.
-- Hence, we don't actually need to build reactive programming in the style like Rx,
-- which comes along with scheduler. (Rx = /asynchronous dataflow/ + /coroutines/)
-- 
-- EVar compose different haskell threads into signal network.
-- 
-- == Example
-- 
-- @
-- main = do
--   evar <- newEVar
--   forM [1..5] $ \num -> forkIO $ emit num evar
--   -- We can get signal for free now.
--   let sig1 = events evar
--   let sig2 = foldp (+) 0 sig1
--   react sig2 print
-- @
--
-- == Notice
--
-- Currently, the EVar should be carefully maintained lifecycle to be avoid garbage collected,
-- due to we rely on pipes-concurrency. 
-- Otherwise, the events signal may be terminated.
-- 
-- See 'Timer' implementation as reference.
--
-- We should lift this restriction in the future.
newtype EVar a = EVar (PC.Output a, PC.Input a, STM ())

-- | Create a new EVar.
-- 
-- Note that the lifecycle of EVar should be greater than deriving Signal.
-- Once the EVar gets garbage collected, the deriving signal will be terminated as well.
newEVar :: IO (EVar a)
newEVar = do
  (output, input, seal) <- PC.spawn' PC.unbounded
  return $ EVar (output, input, seal)

-- | Emit value to EVar, typically in a callback function.
emit :: a -> EVar a -> IO ()
emit val (EVar (output, _, _)) = runEffect $ yield val >-> PC.toOutput output

-- | Convert EVar to Signal.
--
-- Problem: if output is terminated, the input will be terminated as well.
--
-- FIXME - make this sharing.
events :: MonadIO m => EVar a -> Signal m a
events (EVar (_, input, _)) = Signal $ PC.fromInput input

-- | React Signal to perform side effects.
react :: MonadIO m => Signal m a -> (a -> IO ()) -> m ()
react (Signal p) f = runEffect $ for p $ \v -> liftIO $ f v

-- | Explicitly cancel EVar.
cancel :: EVar a -> IO ()
cancel (EVar (_, _, seal)) = atomically seal

type Map = Map.Map

-- | Reference value for indexing BVar.
-- Similar to topic in topic-based publish-subscribe system.
newtype Bref = Bref String deriving (Eq, Ord, Show)

instance IsString Bref where
  fromString = Bref

bref :: String -> Bref
bref = Bref

-- | Abstraction for simple homogeneous collection of EVars,
-- in other words, all EVar in this collection should have same type.
--
-- This is useful when you need to perform publish/subscribe patterns
-- in Haskell thread-per-connection model.
newtype BVar a = BVar (TVar (Map Bref (EVar a)))

-- | Create a BVar which contains empty evars.
newBVar :: IO (BVar a)
newBVar = BVar <$> newTVarIO Map.empty

-- | Consruct BVar from list of Bref and EVar pairs.
fromList :: [(Bref, EVar a)] -> IO (BVar a)
fromList xs = BVar <$> newTVarIO (Map.fromList xs)

-- | A combination of 'register' and 'derefBVar'.
-- In many case, these commands can be combined together,
-- and we encourage to use this first.
eventsB :: MonadIO m => Bref -> EVar a -> BVar a -> IO (Signal m a)
eventsB k v bvar = do
  register k v bvar
  return (events v)

-- | Emit a value to BVar.
emitB :: Bref -> a -> BVar a -> IO (Maybe a)
emitB k val (BVar tvar) = do
  m <- readTVarIO tvar
  case Map.lookup k m of
    Just evar -> emit val evar >> return (Just val)
    Nothing   -> return Nothing

-- | Register an EVar with reference value to BVar.
register :: Bref -> EVar a -> BVar a -> IO ()
register k v (BVar tvar) = atomically $ modifyTVar tvar (Map.insert k v)

-- | Generate Signal from BVar with a specific Bref.
derefBVar :: MonadIO m => Bref -> BVar a -> IO (Maybe (Signal m a))
derefBVar k (BVar tvar) = do
  m <- readTVarIO tvar
  return (events <$> Map.lookup k m)
