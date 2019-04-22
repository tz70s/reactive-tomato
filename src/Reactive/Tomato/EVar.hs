module Reactive.Tomato.EVar
  ( EVar
  , newEVar
  , emit
  , events
  , react
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

import Reactive.Tomato.Event

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
--   -- We can get event for free now.
--   let e1 = events evar
--   react e1 print
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
-- Note that the lifecycle of EVar should be greater than deriving Event.
-- Once the EVar gets garbage collected, the deriving signal will be terminated as well.
newEVar :: IO (EVar a)
newEVar = do
  (output, input, seal) <- PC.spawn' PC.unbounded
  return $ EVar (output, input, seal)

-- | Emit value to EVar, typically in a callback function.
emit :: EVar a -> a -> IO ()
emit (EVar (output, _, _)) val = runEffect $ yield val >-> PC.toOutput output

-- | Convert EVar to Event.
--
-- Problem: if output is terminated, the input will be terminated as well.
--
events :: EVar a -> Event a
events (EVar (_, input, _)) = E $ PC.fromInput input

-- | React Event to perform side effects.
react :: Event a -> (a -> IO ()) -> IO ()
react (E es) f = runEffect $ for es $ \v -> liftIO $ f v

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
eventsB :: Bref -> EVar a -> BVar a -> IO (Event a)
eventsB k v bvar = do
  register k v bvar
  return (events v)

-- | Emit a value to BVar.
emitB :: Bref -> a -> BVar a -> IO (Maybe a)
emitB k val (BVar tvar) = do
  m <- readTVarIO tvar
  case Map.lookup k m of
    Just evar -> emit evar val >> return (Just val)
    Nothing   -> return Nothing

-- | Register an EVar with reference value to BVar.
register :: Bref -> EVar a -> BVar a -> IO ()
register k v (BVar tvar) = atomically $ modifyTVar tvar (Map.insert k v)

-- | Generate Event from BVar with a specific Bref.
derefBVar :: Bref -> BVar a -> IO (Maybe (Event a))
derefBVar k (BVar tvar) = do
  m <- readTVarIO tvar
  return (events <$> Map.lookup k m)
