{-# LANGUAGE DerivingVia, PatternSynonyms, ViewPatterns, UnicodeSyntax, DataKinds, TypeFamilies, TypeAbstractions, BlockArguments, FunctionalDependencies, LambdaCase #-}
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

runCore :: Core a -> IO ()
runCore c = void do
  hs <- newIORef M.empty
  mq <- newChan
  distribute `unCore` (mq, hs)
  c `unCore` (mq, hs)

-- | Run a stack of protocols together
interpSystem :: {-[Protocol ()]-} System a -> Core ()
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

-- It's too easy to only dispatch one handler at once, so we'll just do that.
-- The "all handlers optimistically running at once" in STM would be more fun,
-- but would require making ModifyState and GetState only available within
-- handlers or smtg, to be available to execute handlers in STM.

distribute :: Core ()
distribute = Core \(mq, href) -> do
  let loop = unCore distribute (mq, href)
  handlers <- readIORef href
  Msg e t <- readChan mq
  case M.lookup (EK e) handlers of
    Nothing -> {- message lost -} loop
    Just hs -> do
      -- Write to all handlers the same message
      forM_ hs $ \(Some ch) ->
        writeChan (unsafeCoerce ch) t
      loop

--------------------------------------------------------------------------------
-- * Handlers

-- | Map events to their handlers
type Handlers = IORef (Map EventKey [Some Handler])

-- | An event is its own key, but the type is preserved in the type-rep field only.
data EventKey = forall t. EK (Event t)
instance Eq EventKey where
  (==) (EK a) (EK b) = a == b

data Some k = forall t. Some (k t)

-- A handler is a thread reading @t@s from a channel.
type Handler = Chan

-- -- | A handler is a function from some type to a System execution.
-- -- The type of the function argument is given by the type rep of the event key
-- -- that maps to this handler in the handlers map.
-- data Handler = forall t. H (t -> System ())

registerHandler :: ∀ t. Event t -> (t -> System ()) -> Core ()
registerHandler evt f = Core \(mq, href) -> do
  ch <- newChan @t

  _ <- forkIO $
    let loop = do
          v <- readChan ch
          interpSystem (f v) `unCore` (mq, href)
     in loop

  let ek = EK evt
      h  = Some ch
  modifyIORef' href (M.insertWith (<>) ek [h])

queueMessage :: Event t -> t -> Core ()
queueMessage evt t = Core \(q, _) -> writeChan q (Msg evt t)

--------------------------------------------------------------------------------

newtype Core a = Core { unCore :: (MsgQueue, Handlers) -> IO a } -- needs to be STM since the handlers run atomically...
  deriving (Functor, Applicative, Monad, MonadIO) via (ReaderT (MsgQueue, Handlers) IO)

type MsgQueue = Chan Msg

data Msg = forall t. Msg (Event t) t

