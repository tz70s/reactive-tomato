{-# LANGUAGE ScopedTypeVariables #-}
module Reactive.Tomato.Event
  ( Event
  , generate
  , union
  , unionAll
  , take
  , interpret
  , EVar
  , newEVar
  , emit
  , events
  , react
  , Cell
  , newCell
  , changes
  )
where

import Control.Concurrent hiding (yield)
import Control.Concurrent.Async hiding (cancel)
import Control.Concurrent.STM
import Control.Monad (forM, forM_)
import Pipes
import Prelude hiding (take)

import qualified Pipes.Concurrent as PC
import qualified Pipes.Prelude as PP


-- Revision abstraction.
-- Make the calls safer.

-- 1. Event can't be shared (recomputation), all events should generate via EVars from callbacks.
-- 2. Stateful unit is rename to Cell (a.k.a Signal), but has much limit interface.
-- 3. Change propagation is interop with Event.

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

generate :: [a] -> Event a
generate = E . go where go = foldr ((>>) . yield) mempty

-- | Interpret signal into list within monad context, useful to inspecting signal transformation.
--
-- Note that this is intentionally used in testing.
interpret :: Event a -> IO [a]
interpret (E es) = PP.toListM es

union :: Event a -> Event a -> Event a
union s1 s2 = unionAll [s1, s2]

unionAll :: Traversable t => t (Event a) -> Event a
unionAll xs = E $ do
  (output, input) <- liftIO $ PC.spawn PC.unbounded
  as              <- liftIO $ forM xs $ \(E as) -> async $ do
    runEffect $ as >-> PC.toOutput output
    PC.performGC
  PC.fromInput input
  liftIO $ mapM_ wait as

-- | Take bounded elements from signal, then terminate it.
take :: Int -> Event a -> Event a
take times (E es) = E $ es >-> PP.take times

newtype EVar a = EVar (PC.Output a, PC.Input a, STM ())

newEVar :: IO (EVar a)
newEVar = do
  (output, input, seal) <- PC.spawn' PC.unbounded
  return $ EVar (output, input, seal)

emit :: EVar a -> a -> IO ()
emit (EVar (output, _, _)) val = runEffect $ yield val >-> PC.toOutput output

events :: EVar a -> Event a
events (EVar (_, input, _)) = E $ PC.fromInput input

react :: Event a -> (a -> IO ()) -> IO ()
react (E es) f = runEffect $ for es $ \v -> liftIO $ f v

-- | Now idea to close the thread id properly.
data Cell a = Cell { cache :: TVar a, latches :: TVar [EVar a], tid :: ThreadId }

-- Thought: we can react to pump, change tha cache value and propagate to latches.
-- Maybe we can return IO (Cell a), fork a thread and use it to generate events.
-- ThreadId will be cached then we can kill it when we need?
newCell :: a -> Event a -> IO (Cell a)
newCell init pump' = do
  cache' <- newTVarIO init
  latch' <- newTVarIO []
  tid'   <- forkIO $ react pump' (reaction cache' latch')
  return (Cell cache' latch' tid')
 where
  reaction cache' latch' val = do
    xs <- atomically $ modifyTVar cache' (const val) >> readTVar latch'
    forM_ xs $ \x -> emit x val

cancel :: Cell a -> IO ()
cancel (Cell _ _ tid) = killThread tid

-- | Generate events when a cell gets changes.
changes :: Cell a -> Event a
changes (Cell _ ls _) = E $ do
  evar <- liftIO newEVar
  liftIO $ atomically $ modifyTVar ls (evar :)
  unE $ events evar
