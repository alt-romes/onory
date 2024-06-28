{-# LANGUAGE OverloadedRecordDot, DerivingVia, PatternSynonyms, ViewPatterns, UnicodeSyntax, DataKinds, TypeFamilies, TypeAbstractions, BlockArguments, FunctionalDependencies, LambdaCase, MagicHash #-}
module System.Spec.Interpret
  ( runCore, interpSystem, wait, Verbosity
  ) where

import GHC.Exts (dataToTag#, Int(I#))
import GHC.Records
import Data.IORef
import Control.Monad.Reader
import Type.Reflection
import Control.Monad
import Control.Monad.Free

-- import Data.Set (Set)
-- import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M

import Control.Concurrent.STM
import Control.Concurrent.MVar
import Control.Concurrent
import Unsafe.Coerce (unsafeCoerce)
import System.Random (randomRIO)
import Data.Time.Clock (getCurrentTime)

import System.Spec.Free

--------------------------------------------------------------------------------
-- * Interpreters

-- | Run a stack of protocols together
-- runTower :: [Protocol] -> IO ()

runCore :: Verbosity -> Core a -> IO (Sync, MsgQueue)
runCore v c = do
  hs <- newIORef M.empty
  mq <- newChan
  sync <- newEmptyMVar
  let coreData = CoreData mq hs v
  _ <- forkFinally (worker `unCore` coreData) (\_ -> putMVar sync ())
  _ <- c `unCore` coreData
  return (Sync sync, mq)

interpSystem :: System a -> Core ()
interpSystem sys = void do

  let runF :: SystemF (Core a) -> Core a
      runF = \case
        UponEvent x impl next -> do
          let tr = traceStr 3 ("Handling " ++ show x)
          registerHandler x (\a -> tr >> impl a) >> next
        TriggerEvent x e next -> do
          interpSystem $ traceStr 3 ("Triggering " ++ show x) -- we could automatically show the argument if we required `Show`.
          queueMessage x e
          next
        GetSelf c -> c "localhost"
        MkNew i c -> c . Mutable =<< liftIO (newIORef i)
        ModifyState (Mutable a) b n -> liftIO (modifyIORef' a b) >> n
        GetState (Mutable a) n -> liftIO (readIORef a) >>= n
        GetRandom bnds n -> liftIO (randomRIO bnds) >>= n
        SetupTimer tt evt timer n -> do
          interpSystem $ traceStr 3 ("Setting up " ++ show evt)
          startTimer tt evt timer
          n
        CancelTimer evt n -> do
          interpSystem $ traceStr 3 ("Handling " ++ show evt)
          stopTimer evt
          n
        TraceStr v str n -> do
          verb <- asks verbosity
          time <- liftIO getCurrentTime
          if v <= verb
            then liftIO (putStrLn (show time ++ ": " ++ str)) >> n
            else n

  iterM runF sys

worker :: Core ()
worker = Core \cd -> do
  let loop = unCore worker cd
  handlers <- readIORef cd.handlers

  -- Waits for message
  Msg e t <- readChan cd.msgQueue

  case M.lookup (EK e) handlers of
    Nothing -> {- message lost -} loop
    Just hs -> do
      -- Run all handlers with the same message
      forM_ hs \(H h) -> unsafeCoerce h t
      loop

newtype Sync = Sync (MVar ())

wait :: Sync -> IO ()
wait (Sync m) = readMVar m

type Verbosity = Int

--------------------------------------------------------------------------------
-- * Impl

-- | Map events to their handlers
type Handlers = IORef (Map EventKey [Handler])

-- -- A handler is a thread reading @t@s from a channel.
-- type Handler = Chan

-- | A handler is a function from some type to a System execution.
-- The type of the function argument is given by the type rep of the event key
-- that maps to this handler in the handlers map.
data Handler = forall t. H (t -> IO ())

registerHandler :: ∀ t a. Event t -> (t -> System a) -> Core ()
registerHandler evt f = Core \cd -> do
  let ek = EK evt
      h  = H $ (`unCore` cd) . interpSystem <$> f
  modifyIORef' cd.handlers (M.insertWith (<>) ek [h])

queueMessage :: Event t -> t -> Core ()
queueMessage evt t = Core \cd -> writeChan cd.msgQueue (Msg evt t)

-- | Start a timer.
--
-- NB: We create a handler automatically for a special event called stop timer
-- This handler will kill the thread assign to the relevant timer.
startTimer :: HasField "time" timer Int => TimerType timer -> Event timer -> timer -> Core ()
startTimer tt evt@(Timer tr) timer = Core \cd -> do
  let registerCancelTimer tid = modifyIORef' cd.handlers (M.insertWith (<>) (EK (StopTimer tr)) [cancelTimerThread tid])
  case tt of
    OneShotTimer -> do
      tid <- forkIO $ do
        threadDelay (timer.time * 1000)
        queueMessage evt timer `unCore` cd
      registerCancelTimer tid
    PeriodicTimer -> do
      tid <- forkIO $ do
        -- Delay until first occurrence
        threadDelay (timer.time * 1000)
        queueMessage evt timer `unCore` cd
        -- Periodically repeat
        let loop = do
              threadDelay (timer.repeat * 1000)
              queueMessage evt timer `unCore` cd
              loop
         in loop
      registerCancelTimer tid
startTimer _ _ _ = error "Setup timer should only be called on a timer event"

stopTimer :: Event timer -> Core ()
stopTimer (Timer t) = queueMessage (StopTimer t) (error "Upon StopTimer event should not be possible to define")
stopTimer _ = error "Cancel timer should only be called on timer events"

cancelTimerThread :: ThreadId -> Handler
cancelTimerThread tid = H $ \_ -> killThread tid

--------------------------------------------------------------------------------

type MsgQueue = Chan Msg
data Msg = forall t. Msg (Event t) t

--------------------------------------------------------------------------------
-- * Core monad

newtype Core a = Core { unCore :: CoreData -> IO a } -- needs to be STM since the handlers run atomically...
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader CoreData) via (ReaderT CoreData IO)

data CoreData = CoreData
  { msgQueue :: MsgQueue
  , handlers :: Handlers
  , verbosity :: Verbosity
  }

--------------------------------------------------------------------------------
-- * EventKey

-- | An event is its own key, but the type is preserved in the type-rep field only.
data EventKey = forall t. EK (Event t)

instance Eq EventKey where
  (==) (EK a) (EK b) =
    case (a, b) of
      (Message r1, Message r2) -> SomeTypeRep r1 == SomeTypeRep r2
      (Timer r1, Timer r2) -> SomeTypeRep r1 == SomeTypeRep r2
      (StopTimer r1, StopTimer r2) -> SomeTypeRep r1 == SomeTypeRep r2
      (Request n1 r1, Request n2 r2) -> n1 == n2 && SomeTypeRep r1 == SomeTypeRep r2
      (Indication n1 r1, Indication n2 r2) -> n1 == n2 && SomeTypeRep r1 == SomeTypeRep r2
      (_, _) -> False

instance Ord EventKey where
  compare (EK a) (EK b) =
    case (a, b) of
      (Message r1, Message r2) -> SomeTypeRep r1 `compare` SomeTypeRep r2
      (Timer r1, Timer r2) -> SomeTypeRep r1 `compare` SomeTypeRep r2
      (StopTimer r1, StopTimer r2) -> SomeTypeRep r1 `compare` SomeTypeRep r2
      (Request n1 r1, Request n2 r2) -> n1 `compare` n2 <> SomeTypeRep r1 `compare` SomeTypeRep r2
      (Indication n1 r1, Indication n2 r2) -> n1 `compare` n2 <> SomeTypeRep r1 `compare` SomeTypeRep r2
      (_, _) -> I# (dataToTag# a) `compare` I# (dataToTag# b)

