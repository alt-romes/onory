{-# LANGUAGE OverloadedRecordDot, DerivingVia, PatternSynonyms, ViewPatterns, UnicodeSyntax, DataKinds, TypeFamilies, TypeAbstractions, BlockArguments, FunctionalDependencies, LambdaCase, MagicHash #-}
module System.Distributed.Interpret
  ( runTower, runCore, interpSystem, wait, Verbosity
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

import Control.Concurrent.MVar
import Control.Concurrent
import Unsafe.Coerce (unsafeCoerce)
import System.Random (randomRIO)
import Data.Time.Clock (getCurrentTime)

import System.Distributed.Free

--------------------------------------------------------------------------------
-- * Interpreter

-- | Run a stack of protocols together
runTower :: Verbosity -> [Protocol] -> IO ()
runTower v protos = do
  sync <- runCore v $ interpSystem $ sequence protos
  wait sync

runCore :: Verbosity -> Core a -> IO Sync
runCore v c = do
  hs <- newIORef M.empty
  mq <- newChan
  sync <- newEmptyMVar
  let coreData = CoreData mq hs TopLevel v
  _ <- forkFinally (worker `unCore` coreData) (\_ -> putMVar sync ())
  _ <- c `unCore` coreData
  return (Sync sync)

interpSystem :: System a -> Core ()
interpSystem sys = void do

  let runF :: SystemF (Core a) -> Core a
      runF = \case
        ProtocolBoundary i p n      -> interpProtocol i p                                      >>  n
        UponEvent x impl n          -> registerHandler x (\a ->
                                       traceStr 3 ("Handling " ++ show x) >> impl a)           >>  n
        TriggerEvent x e n          -> trace' "Triggering " x    >> queueMessage x e           >>  n
        SetupTimer tt evt timer n   -> trace' "Setting up " evt  >> startTimer tt evt timer    >>  n
        CancelTimer evt n           -> trace' "Handling " evt    >> stopTimer evt              >>  n
        ModifyState (Mutable a) b n -> liftIO (modifyIORef' a b)                               >>  n
        MkNew i n                   -> liftIO (Mutable <$> newIORef i)                         >>= n
        GetState (Mutable a) n      -> liftIO (readIORef a)                                    >>= n
        GetRandom bnds n            -> liftIO (randomRIO bnds)                                 >>= n
        EscapeTheSystem io n        -> liftIO io                                               >>= n
        TraceStr v str n            -> trace v str                                             >>  n
        GetSelf n                   -> n "localhost"

      trace' kw x = trace 3 (kw ++ show x)
         -- we could automatically show the argument if we required `Show`.

  iterM runF sys

-- | The core worker reads messages from the event queue and delegates them to
-- the appropriate handler.
--
-- Handlers from different protocols may be run in parallel. To achieve this,
-- each protocol declaration starts an "executor" thread which reads from a
-- channel the handlers to run in sequence. The worker writes the saturated
-- handler action to run into a channel where it will get picked up by the
-- executor.
worker :: Core ()
worker = Core \cd -> do
  topLevelExec <- newExecutor -- for top-level handlers

  forever $ do
    -- Waits for message
    Msg e t <- readChan cd.msgQueue

    handlers <- readIORef cd.handlers
    case M.lookup (EK e) handlers of
      Nothing ->
        {- message ignored -}
        traceInternal cd $
          "Ignored event " ++ show e ++ " given handlers " ++ show (M.toList handlers)
      Just hs -> do
        -- Run all handlers with the same message by passing them to the appropriate executor
        forM_ hs \(exectx, H h) -> do
          writeChan (case exectx of TopLevel -> topLevelExec; Scoped _ protoc -> protoc)
            (unsafeCoerce h t)

wait :: Sync -> IO ()
wait (Sync m) = readMVar m

registerHandler :: ∀ t a. Event t -> (t -> System a) -> Core ()
registerHandler evt f = Core \cd -> do
  traceInternal cd $ "Registering handler for " ++ show evt
  let ek = EK evt
      h  = H $ (`unCore` cd) . interpSystem <$> f
  modifyIORef' cd.handlers (M.insertWith (<>) ek [(cd.inProto,h)])

queueMessage :: Event t -> t -> Core ()
queueMessage evt t = Core \cd -> writeChan cd.msgQueue (Msg evt t)

-- | Start a timer.
--
-- NB: We create a handler automatically for a special event called stop timer
-- This handler will kill the thread assign to the relevant timer.
startTimer :: HasField "time" timer Int => TimerType timer -> Event timer -> timer -> Core ()
startTimer tt evt@(Timer tr) timer = Core \cd -> do
  let
    -- Even though this timer may have been started within a particular protocol, any other protocol can register a handler for it.
    -- To preserve some symmetry, the handler that cancels the timer shall be excuted in the top-level executor
    registerCancelTimerH tid = modifyIORef' cd.handlers (M.insertWith (<>) (EK (StopTimer tr)) [(TopLevel, cancelTimerThread tid)])
      
  case tt of
    OneShotTimer -> do
      tid <- forkIO $ do
        threadDelay (timer.time * 1000)
        queueMessage evt timer `unCore` cd
      registerCancelTimerH tid
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
      registerCancelTimerH tid
startTimer _ _ _ = error "Setup timer should only be called on a timer event"

stopTimer :: Event timer -> Core ()
stopTimer (Timer t) = queueMessage (StopTimer t) (error "Upon StopTimer event should not be possible to define")
stopTimer _ = error "Cancel timer should only be called on timer events"

cancelTimerThread :: ThreadId -> Handler
cancelTimerThread tid = H $ \_ -> killThread tid

trace :: Verbosity -> String -> Core ()
trace v str = do
  verb <- asks verbosity
  prnm <- asks inProto
  time <- liftIO getCurrentTime
  when (v <= verb) $
    liftIO (putStrLn (show time ++ ": (" ++ show prnm ++ ") " ++ str))

traceInternal :: CoreData -> String -> IO ()
traceInternal c s = trace 5 s `unCore` c

-- | Basically 'interpSystem', but sets the (new) protocol executor in the local env
interpProtocol :: Name -> Protocol -> Core ()
interpProtocol name proto = do
  protoExec <- liftIO newExecutor
  local (\cd -> case cd.inProto of
    TopLevel -> cd{inProto=Scoped name protoExec}
    Scoped{} -> error "Nested protocol declarations are not supported!") $
      interpSystem proto

newExecutor :: IO (Chan (IO ()))
newExecutor = do
  ch <- newChan
  _ <- forkIO $ forever $ join (readChan ch) -- read an action from the channel and execute it forever
  return ch

--------------------------------------------------------------------------------
-- * Core datatypes

-- | Map events to their handlers and the protocol under which they were registered.
--
-- The protocol under which a handler is registered matters because handlers in
-- different protocols can be run simultaneously (see 'worker').
--
-- If we are under no protocol the action is executed by the top-level executor
type Handlers = IORef (Map EventKey [(ExecutorContext, Handler)])

-- -- A handler is a thread reading @t@s from a channel.
-- type Handler = Chan

-- | A handler is a function from some type to a System execution.
-- The type of the function argument is given by the type rep of the event key
-- that maps to this handler in the handlers map.
data Handler = forall t. H (t -> IO ())

data ExecutorContext = TopLevel | Scoped Name ProtocolKey

-- | A protocol key is the channel from which the /executor/ of the protocol
-- will read saturated handler actions.
type ProtocolKey = Chan (IO ())

type MsgQueue = Chan Msg
data Msg = forall t. Msg (Event t) t

newtype Core a = Core { unCore :: CoreData -> IO a } -- needs to be STM since the handlers run atomically...
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader CoreData) via (ReaderT CoreData IO)

data CoreData = CoreData
  { msgQueue :: MsgQueue
  , handlers :: Handlers
  , inProto  :: ExecutorContext -- ^ The protocol we are under, to tag handlers. TopLevel if not under a protocol.
  , verbosity :: Verbosity
  }

-- | An event is its own key, but the type is preserved in the type-rep field only.
data EventKey = forall t. EK (Event t)

newtype Sync = Sync (MVar ())

-- data CoreConfig = CoreConfig { channel type, verbosity, ... }

--------------------------------------------------------------------------------
-- * Instances

instance Show Handler where
  show _ = "<handler>"

deriving instance Show EventKey

instance Show ExecutorContext where
  show TopLevel = "Core"
  show (Scoped n _) = n

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

