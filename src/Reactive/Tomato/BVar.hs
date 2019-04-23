module Reactive.Tomato.BVar
  ( Bref
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

import Reactive.Tomato.Event
import Reactive.Tomato.EVar

import qualified Data.Map as Map

type Map = Map.Map

-- | Reference value for indexing BVar.
-- Similar to topic in topic-based publish-subscribe system.
newtype Bref = Bref String deriving (Eq, Ord, Show)

instance IsString Bref where
  fromString = Bref

bref :: String -> Bref
bref = Bref
{-# INLINABLE bref #-}

-- | Abstraction for simple homogeneous collection of EVars,
-- in other words, all EVar in this collection should have same type.
--
-- This is useful when you need to perform publish/subscribe patterns
-- in Haskell thread-per-connection model.
newtype BVar a = BVar (TVar (Map Bref (EVar a)))

-- | Create a BVar which contains empty evars.
newBVar :: IO (BVar a)
newBVar = BVar <$> newTVarIO Map.empty
{-# INLINABLE newBVar #-}

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
{-# INLINABLE eventsB #-}

-- | Emit a value to BVar.
emitB :: Bref -> a -> BVar a -> IO (Maybe a)
emitB k val (BVar tvar) = do
  m <- readTVarIO tvar
  case Map.lookup k m of
    Just evar -> emit evar val >> return (Just val)
    Nothing   -> return Nothing
{-# INLINABLE emitB #-}

-- | Register an EVar with reference value to BVar.
register :: Bref -> EVar a -> BVar a -> IO ()
register k v (BVar tvar) = atomically $ modifyTVar tvar (Map.insert k v)
{-# INLINABLE register #-}

-- | Generate Event from BVar with a specific Bref.
derefBVar :: Bref -> BVar a -> IO (Maybe (Event a))
derefBVar k (BVar tvar) = do
  m <- readTVarIO tvar
  return (events <$> Map.lookup k m)
{-# INLINABLE derefBVar #-}
