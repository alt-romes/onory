{-# LANGUAGE OverloadedRecordDot, DerivingVia, PatternSynonyms, ViewPatterns, UnicodeSyntax, DataKinds, TypeFamilies, TypeAbstractions, BlockArguments, FunctionalDependencies, LambdaCase #-}
module System.Spec.Interpret
  ( runCore, interpSystem
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
import Unsafe.Coerce (unsafeCoerce)

--------------------------------------------------------------------------------
-- * Interpreters

-- | Run a stack of protocols together
-- runTower :: [Protocol] -> IO ()

runCore :: Core a -> IO MsgQueue
runCore c = do
  hs <- newIORef M.empty
  mq <- newChan
  forkIO $
    worker `unCore` (mq, hs)
  _ <- c `unCore` (mq, hs)
  return mq

interpSystem :: System a -> Core ()
interpSystem sys = void do

  let runF :: SystemF (Core a) -> Core a
      runF = \case
        UponEvent x impl next -> registerHandler x impl >> next
        TriggerEvent x e next -> queueMessage x e >> next
        GetSelf c -> c "localhost"
        MkNew i c -> c . Mutable =<< liftIO (newIORef i)
        ModifyState (Mutable a) b n -> liftIO (modifyIORef' a b) >> n
        GetState (Mutable a) n -> liftIO (readIORef a) >>= n

  iterM runF sys

worker :: Core ()
worker = Core \(mq, href) -> do
  let loop = unCore worker (mq, href)
  handlers <- readIORef href

  -- Waits for message
  Msg e t <- readChan mq

  case M.lookup (EK e) handlers of
    Nothing -> {- message lost -} loop
    Just hs -> do
      -- Run all handlers with the same message
      forM_ hs \(H h) -> unsafeCoerce h t
      loop

--------------------------------------------------------------------------------
-- * Handlers

-- | Map events to their handlers
type Handlers = IORef (Map EventKey [Handler])

-- -- A handler is a thread reading @t@s from a channel.
-- type Handler = Chan

-- | A handler is a function from some type to a System execution.
-- The type of the function argument is given by the type rep of the event key
-- that maps to this handler in the handlers map.
data Handler = forall t. H (t -> IO ())

registerHandler :: ∀ t a. Event t -> (t -> System a) -> Core ()
registerHandler evt f = Core \(mq, href) -> do
  let ek = EK evt
      h  = H $ (`unCore` (mq, href)) . interpSystem <$> f
  modifyIORef' href (M.insertWith (<>) ek [h])

queueMessage :: Event t -> t -> Core ()
queueMessage evt t = Core \(q, _) -> writeChan q (Msg evt t)

--------------------------------------------------------------------------------

type MsgQueue = Chan Msg
data Msg = forall t. Msg (Event t) t

--------------------------------------------------------------------------------
-- * Core monad

newtype Core a = Core { unCore :: (MsgQueue, Handlers) -> IO a } -- needs to be STM since the handlers run atomically...
  deriving (Functor, Applicative, Monad, MonadIO) via (ReaderT (MsgQueue, Handlers) IO)

--------------------------------------------------------------------------------
-- * EventKey

-- | An event is its own key, but the type is preserved in the type-rep field only.
data EventKey = forall t. EK (Event t)

instance Eq EventKey where
  (==) (EK a) (EK b)
    | Message r1 <- a
    , Message r2 <- b
    = SomeTypeRep r1 == SomeTypeRep r2
    | otherwise
    = a.name == b.name && SomeTypeRep a.argTy == SomeTypeRep b.argTy

instance Ord EventKey where
  compare (EK a) (EK b)
    | Message r1 <- a
    , Message r2 <- b
    = SomeTypeRep r1 `compare` SomeTypeRep r2
    | otherwise
    = a.name `compare` b.name <> SomeTypeRep a.argTy `compare` SomeTypeRep b.argTy

