{-# LANGUAGE OverloadedRecordDot, DerivingVia, UnicodeSyntax, DataKinds,
   TypeFamilies, TypeAbstractions, BlockArguments, LambdaCase, MagicHash,
   DuplicateRecordFields, RecordWildCards, DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant <$>" #-}
module System.Distributed.Interpret
  ( runLebab, runProtocols, runSystem, runCore, interpSystem, wait, SysConf(..)
  ) where

import GHC.Exts (dataToTag#, Int(I#))
import GHC.Fingerprint (Fingerprint)
import GHC.Records
import Data.Binary (Binary, encode, decode)
import Data.Constraint
import Data.IORef
import Control.Monad.Reader
import Type.Reflection
import Control.Monad
import Control.Monad.Free
import qualified Network.Transport as N
import qualified Network.Transport.TCP as N

import Data.Map (Map)
import qualified Data.Map as M

import Control.Concurrent.MVar
import Control.Concurrent
import Unsafe.Coerce (unsafeCoerce)
import System.Random (randomRIO)
import Data.Time.Clock (getCurrentTime)
import Data.ByteString (ByteString, toStrict, fromStrict)

import System.Distributed.Free

--------------------------------------------------------------------------------
-- * Runners

-- | Run multiple protocols concurrently in harmony
runLebab, runProtocols :: SysConf -> [Protocol] -> IO ()
runLebab = runProtocols
runProtocols conf protos = do
  sync <- runCore conf $ interpSystem $ sequence protos
  wait sync

runSystem :: SysConf -> System a -> IO ()
runSystem c s = do
  sync <- runCore c $ interpSystem s
  wait sync -- `onCtrlC` closeTransport transport

data SysConf = SysConf
  { verbosity :: Verbosity
  , hostname :: String
  -- ^ The IP that other nodes will use to connect to it is greatly preferred,
  -- since this allows network-protocol-tcp to re-use the connection.
  , port :: Int
  }

--------------------------------------------------------------------------------
-- * Interpreter

runCore :: SysConf -> Core a -> IO Sync
runCore SysConf{..} c = do
  msgDecoders <- newIORef M.empty
  handlers    <- newIORef M.empty
  msgQueue    <- newChan
  sync        <- newEmptyMVar
  (self, outgoingMsgs) <- initTcpManager verbosity hostname port msgDecoders msgQueue
  let coreData = CoreData { msgQueue, handlers, inProto = TopLevel
                          , verbosity, outgoingMsgs, self, msgDecoders }
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
        TriggerEvent x e n          -> trace' "Triggering " x    >> queueEvent x e           >>  n
        SetupTimer tt evt timer n   -> trace' "Setting up " evt  >> startTimer tt evt timer    >>  n
        CancelTimer evt n           -> trace' "Handling " evt    >> stopTimer evt              >>  n
        ModifyState (Mutable a) b n -> liftIO (modifyIORef' a b)                               >>  n
        MkNew i n                   -> liftIO (Mutable <$> newIORef i)                         >>= n
        GetState (Mutable a) n      -> liftIO (readIORef a)                                    >>= n
        GetRandom bnds n            -> liftIO (randomRIO bnds)                                 >>= n
        EscapeTheSystem io n        -> liftIO io                                               >>= n
        TraceStr v str n            -> trace v str                                             >>  n
        GetSelf n                   -> n =<< asks self

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

  -- Handlers registered for Messages must also store the decoder function (see
  -- 'MsgDecoders') since messages come from the network with a type fingerprint only.
  case evt of
    Message{tyId} -> {- matching on Message gives us a Binary instance for @t@ -}
      modifyIORef' cd.msgDecoders (M.insert tyId (MDEC @t (ndecode, Dict)))
        -- insert may override the existing decoder if multiple handlers for the
        -- same message are defined, but this is fine since the decoder will always
        -- be the same (the Binary instance decoder)
    _ -> pure ()


queueEvent :: Event t -> t -> Core ()
queueEvent evt t = Core \cd ->
  -- For Messages, queue them onto the outgoing channel instead of the core msg queue
  case evt of
    Message{} -> writeChan cd.outgoingMsgs (Msg evt t)
    _ -> writeChan cd.msgQueue (Msg evt t)

-- | Start a timer.
--
-- NB: We create a handler automatically for a special event called stop timer
-- This handler will kill the thread assign to the relevant timer.
startTimer :: TimerType timer -> Event timer -> timer -> Core ()
startTimer tt evt@(Timer tr) timer = Core \cd -> do
  let
    -- Even though this timer may have been started within a particular protocol, any other protocol can register a handler for it.
    -- To preserve some symmetry, the handler that cancels the timer shall be excuted in the top-level executor
    registerCancelTimerH tid = modifyIORef' cd.handlers (M.insertWith (<>) (EK (StopTimer tr)) [(TopLevel, cancelTimerThread tid)])
      
  case tt of
    OneShotTimer -> do
      tid <- forkIO $ do
        threadDelay (timer.time * 1000)
        queueEvent evt timer `unCore` cd
      registerCancelTimerH tid
    PeriodicTimer -> do
      tid <- forkIO $ do
        -- Delay until first occurrence
        threadDelay (timer.time * 1000)
        queueEvent evt timer `unCore` cd
        -- Periodically repeat
        let loop = do
              threadDelay (timer.repeat * 1000)
              queueEvent evt timer `unCore` cd
              loop
         in loop
      registerCancelTimerH tid
startTimer _ _ _ = error "Setup timer should only be called on a timer event"

stopTimer :: Event timer -> Core ()
stopTimer (Timer t) = queueEvent (StopTimer t) (error "Upon StopTimer event should not be possible to define")
stopTimer _ = error "Cancel timer should only be called on timer events"

cancelTimerThread :: ThreadId -> Handler
cancelTimerThread tid = H $ \_ -> killThread tid

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
-- * Trace

trace :: Verbosity -> String -> Core ()
trace v str = do
  verb <- asks (.verbosity)
  prnm <- asks inProto
  liftIO $ traceIO v str verb (show prnm)

traceInternal :: CoreData -> String -> IO ()
traceInternal c s = trace internalVerbosity s `unCore` c

traceIO :: Verbosity -> String -> (Verbosity -> String {- context -} -> IO ())
traceIO v str verb ctx = do
  time <- liftIO getCurrentTime
  when (v <= verb) $
    liftIO (putStrLn (show time ++ ": (" ++ ctx ++ ") " ++ str))

internalVerbosity :: Verbosity
internalVerbosity = 4

--------------------------------------------------------------------------------
-- * Network

-- network-transport(-tcp) take care of reusing transport between two hosts and
-- providing lightweight connections between endpoints (see
-- https://hackage.haskell.org/package/network-transport-tcp/docs/Network-Transport-TCP.html)

-- | Manages TCP connections to other Hosts.
--
-- Starts a thread which reads @send@ messages from a channel and executes on
-- them (sending them to other hosts).
-- Another thread receives messages from other hosts and queues them onto the
-- message queue being managed by the Core worker to be handled by the protocols.
--
-- Returns the channel from which it reads messages to send to other hosts, and this system's host.
-- Receives the channel onto which it will write message-received and channel events.
initTcpManager :: Verbosity -> String -> Int -> MsgDecoders -> MsgQueue -> IO (Host, MsgQueue)
initTcpManager v hostname port decoders coreMsgQ = do
  -- The connection will only be reused if the announced address when
  -- connecting to the other node matches the address the other node uses to
  -- connect to this node.
  let netTrace s = traceIO internalVerbosity s v "Network"

  -- todo: getAddr. For now, use localhost on different ports
  Right transport <- N.createTransport (N.defaultTCPAddr hostname (show port)) N.defaultTCPParameters
  Right endpoint <- N.newEndPoint transport

  -- Write outgoing messages to me, and I'll send them!
  outgoingMsgs <- newChan

  -- Incoming side (receives connections)
  _ <- forkIO $ forever $ do
    event <- N.receive endpoint
    case event of
      N.Received _ [msgTyFp, msgTyStr, msgPayload] -> do
        let tyId  = ndecode msgTyFp
            tyStr = ndecode msgTyStr
        M.lookup tyId <$> readIORef decoders >>= \case
          Nothing ->
            netTrace $ "No decoder found for " ++ show tyStr ++ ". Ignoring message..."
          Just (MDEC (decoder, dict)) -> withDict dict $
            -- Write the received message into the core message queue from
            -- where it will be dequeued and processed by the appropriate handlers
            writeChan coreMsgQ (Msg Message{tyId, tyStr} (decoder msgPayload))
      N.Received _ m ->
        netTrace $ "Unexpected message content: " ++ show m

      N.ConnectionClosed e ->
        -- todo: can this detect outgoing down? or incoming down?
        netTrace ("TODO: Out/InnConnDown connection closed " ++ show e)
      N.ConnectionOpened _ _ ep -> do
        -- Flag in connection up
        writeChan coreMsgQ (Msg (ChannelEvt (typeRep @InConnUp)) InConnUp{from = Host ep})
      N.ReceivedMulticast{} -> do
        netTrace "Ignoring received multicast"
        pure () -- do nothing
      N.EndPointClosed -> do
        netTrace "Endpoint closed?"
        pure () -- ok
      N.ErrorEvent e -> do
        netTrace ("Unexpected error event " ++ show e)

  -- Outgoing side (starts connections)
  _ <- forkIO $ forever $ do
    Msg (Message msgTyFP msgTyStr) msg <- readChan outgoingMsgs
    N.connect endpoint msg.to.addr N.ReliableOrdered N.defaultConnectHints {-todo: set timeout in hints ? -}
      >>= \case
        Left err ->
          -- Flag out connection failed
          writeChan coreMsgQ (Msg (ChannelEvt (typeRep @OutConnFailed)) OutConnFailed{ err, to=msg.to })
        Right outConn -> do
          -- Flag channel up
          writeChan coreMsgQ (Msg (ChannelEvt (typeRep @OutConnUp)) OutConnUp{to = msg.to})
          -- We could store outConn in the map and kind of manage it, but these
          -- connections are supposed to be very light, so there's no need to
          -- cache them in any way unless proven otherwise.
          N.send outConn [nencode msgTyFP, nencode msgTyStr, nencode msg]
            >>= \case
              Left e -> netTrace ("Unexpected error when sending message " ++ show e)
              Right () -> pure ()

  return (Host (N.address endpoint), outgoingMsgs)

-- other channel protocols (e.g. UDP) are not supported

nencode :: Binary a => a -> ByteString
nencode = toStrict . encode

ndecode :: Binary a => ByteString -> a
ndecode = decode . fromStrict

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
  , outgoingMsgs :: MsgQueue
  , self :: Host
  , msgDecoders :: MsgDecoders
  }

-- | We need a way of converting a bytestring we got from the net into a type
-- we can pass to the handler. We assign a fingerprint identifying the type to
-- a decoder function whose result we can "safely" unsafe coerce into the
-- argument expected by the handler for the message whose type has that same
-- fingerprint.
type MsgDecoders = IORef (Map Fingerprint MsgDecoder)
data MsgDecoder = forall a. MDEC (ByteString -> a, Dict (HasField "to" a Host, Binary a))

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
      -- The message case is the most important, since we want to store them in
      -- maps *without* knowing the type rep, only knowing the fingerprint s.t.
      -- messages can be sent to other nodes. Don't force the type rep! It'll
      -- always be \bot for messages coming from other nodes.
      (Message _dont1 f1, Message _dont2 f2) -> f1 == f2
      (Timer r1, Timer r2) -> SomeTypeRep r1 == SomeTypeRep r2
      (StopTimer r1, StopTimer r2) -> SomeTypeRep r1 == SomeTypeRep r2
      (Request n1 r1, Request n2 r2) -> n1 == n2 && SomeTypeRep r1 == SomeTypeRep r2
      (Indication n1 r1, Indication n2 r2) -> n1 == n2 && SomeTypeRep r1 == SomeTypeRep r2
      (_, _) -> False

instance Ord EventKey where
  compare (EK a) (EK b) =
    case (a, b) of
      -- Be careful with messages, don't look at the type rep! See the comment
      -- on the Eq instance above.
      (Message _dont1 f1, Message _dont2 f2) -> f1 `compare` f2
      (Timer r1, Timer r2) -> SomeTypeRep r1 `compare` SomeTypeRep r2
      (StopTimer r1, StopTimer r2) -> SomeTypeRep r1 `compare` SomeTypeRep r2
      (Request n1 r1, Request n2 r2) -> n1 `compare` n2 <> SomeTypeRep r1 `compare` SomeTypeRep r2
      (Indication n1 r1, Indication n2 r2) -> n1 `compare` n2 <> SomeTypeRep r1 `compare` SomeTypeRep r2
      (_, _) -> I# (dataToTag# a) `compare` I# (dataToTag# b)

