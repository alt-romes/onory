{-# LANGUAGE DerivingVia, PatternSynonyms, ViewPatterns, UnicodeSyntax, DataKinds, TypeFamilies, TypeAbstractions, BlockArguments, FunctionalDependencies, LambdaCase #-}
module System.Spec.Interpret
  ( runSystem
  ) where

import Data.IORef
import Control.Monad.Reader
import Type.Reflection
import Data.Kind
import Control.Monad
import Control.Monad.Free

import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M

import System.Spec.Free
import Control.Concurrent.STM
import Control.Concurrent

--------------------------------------------------------------------------------
-- * Interpreters

-- | Run a stack of protocols together
runSystem :: {-[Protocol ()]-} System a -> IO ()
runSystem sys = runCore do

  let runF :: SystemF (Core a) -> Core a
      runF = \case
        UponEvent x impl next -> registerHandler x impl >> next
        TriggerEvent x e next -> queueMessage x e >> next
        GetSelf c -> c "localhost"
        MkNew i c -> c . Mutable =<< liftIO (newTVarIO i)
        ModifyState (Mutable a) b n -> do
          ModifyState a b (f n)
        GetState a n -> GetState a (f . n)

  iterM runF sys

-- Could it ever be that under too much contention an atomic block
-- could be too slow to terminate if enough TVars are involved? My
-- guess is not (we can do thousands of TVar transactions), and not
-- to worry until it is observed.
-- An alternative design is to only dispatch one handler at once.
-- That basically means the handler map has to have each function and invoke
-- it after receiving a message related to it.
-- But the "all handlers optimistically running at once" is more fun.

--------------------------------------------------------------------------------
-- * Handlers

-- | Map events to their handlers
type Handlers = IORef (Map EventKey [Handler])

-- | An event is its own key, but the type is preserved in the type-rep field only.
data EventKey = forall t. EK (Event t)

-- | A handler is a function from some type to a System execution.
-- The type of the function argument is given by the type rep of the event key
-- that maps to this handler in the handlers map.
data Handler = forall t. H (t -> System ())

registerHandler :: Event t -> (t -> System ()) -> Core ()
registerHandler evt f = Core \(_, handlers) -> do
  -- TODO: Make sure handler is not yet registered... otherwise we'll have two
  -- different handlers pulling from a queue with non-duplicate messages...
  let ek = EK evt
      h  = H f
  modifyIORef' handlers (M.insertWith (<>) ek [h])

queueMessage :: Event t -> t -> Core ()
queueMessage evt t = Core \(q, _) -> writeChan q (Msg evt t)

--------------------------------------------------------------------------------

newtype Core a = Core { unCore :: (MsgQueue, Handlers) -> IO a } -- needs to be STM since the handlers run atomically...
  deriving (Functor, Applicative, Monad, MonadIO) via (ReaderT (MsgQueue, Handlers) IO)

type MsgQueue = Chan Msg

data Msg = forall t. Msg (Event t) t

runCore :: Core a -> IO ()
runCore c = void do
  hs <- newIORef M.empty
  mq <- newChan
  unCore c (mq, hs)

