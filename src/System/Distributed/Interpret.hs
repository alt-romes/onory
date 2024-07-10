{-# LANGUAGE OverloadedRecordDot, DerivingVia, UnicodeSyntax, DataKinds,
   TypeFamilies, BlockArguments, LambdaCase, MagicHash,
   DuplicateRecordFields, RecordWildCards, DeriveAnyClass, ImpredicativeTypes,
   PatternSynonyms, CPP #-}
#if __GLASGOW_HASKELL__ >= 908
{-# LANGUAGE TypeAbstractions #-}
#endif
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant <$>" #-}
-- | A 'System' is a specification. This module provides functions to turn a
-- 'System' specification into an executable program, namely, 'runProtos' and 'runSystem'.
--
-- System specifications are also typically parametrised over a set of configuration parameters.
-- 'runProtos' further provides the ability to turn a protocol's configuration
-- options into a command-line interface from which options can be directly
-- parsed into the execution of the program.
module System.Distributed.Interpret
  (
  -- | 'runProtos' and 'runSystem' are the two main entrypoints for interpreting a 'System'
    runProtos, runSystem, readConf

  , SysConf, SysConf'(..), P(..)

  -- * Low-level interface
  -- | You may want to use these primitives for low-level uses of the library.
  , runCore, interpSystem, wait
  ) where

import GHC.Exts (dataToTag#, Int(I#))
import GHC.Fingerprint (Fingerprint)
import GHC.Records
import GHC.Generics
import GHC.TypeLits
import Data.Binary (Binary, encode, decode, decodeOrFail)
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
import Options.Applicative (execParser, info, fullDesc)
import Options.Generic
import System.Distributed.Free
import qualified Data.Char as C
import Data.Proxy
import Data.Traversable (for)

--------------------------------------------------------------------------------
-- * Runners

-- | Run multiple protocols concurrently in harmony, where the configuration
-- for each protocol is parsed from the command line invocation.
--
-- The program when run reads the configuration for the System as a whole
-- ( t'SysConf' ) and the configuration for each protocol from the command
-- line interface which are derived automatically from the datatype (run with
-- @--help@ to see the available options).
--
-- See also how to construct a t'P' from a 'Protocol' in t'P'.
runProtos :: [P] -> IO ()
runProtos protos = do
  let
    -- Parse SysConf and the configuration record for every protocol
    parser = (,) <$>
      parseRecordWithModifiers @(SysConf' Wrapped) lispCaseModifiers <*>
      for protos (\(P @conf @name p) -> do
        SomeP . p . unwrap <$> parseRecordWithModifiers @(conf Wrapped) (mods (Proxy @name)))
      -- NB: Merging all these generic parsers means we will get multiple "-h" options. Fine.
    mods p =
      lispCaseModifiers {fieldNameModifier = ((map C.toLower (symbolVal p) ++ "-") ++) . lispCaseModifiers.fieldNameModifier}
  (conf, instProtos) <- execParser (info parser fullDesc)
  sync <- runCore (unwrap conf) $ interpSystem $ mapM (\(SomeP p) -> void p) instProtos
  wait sync

-- | Interpret and execute a system given a system configuration.
-- If you rather want an automatically cli-configurable stack of protocols you
-- may prefer 'runProtos'.
runSystem :: SysConf -> System a -> IO ()
runSystem c s = do
  sync <- runCore c $ interpSystem s
  wait sync -- `onCtrlC` closeTransport transport

-- | Reads a configuration record from the command line interface. Useful when
-- using 'runSystem' rather than 'runProtos'.
readConf :: ∀ c. ( GenericParseRecord (Rep (c Wrapped)), Unwrappable c ) => IO (c Unwrapped)
readConf = execParser (info (unwrap <$> parseRecordWithModifiers @(c Wrapped) lispCaseModifiers) fullDesc)

-- | A system configuration record with the CLI-relevant additional information dropped.
-- This means that @verbosity :: Int@, @hostname :: String@, and @port :: Int@.
type SysConf = SysConf' Unwrapped

-- | A system configuration record with additional information to report
-- helpful information on @--help@ in the command line interface, specify
-- command line interface shorter opts, and default values.
--
-- Verbosity can be specified using @-V@ if using the command line interface
-- when using 'System.Distributed.Interpret.runProtos', or in the @verbosity@
-- field of t'System.Distributed.Interpret.SysConf' when using
-- 'System.Distributed.Interpret.runSystem'.
--
-- === Verbosity levels
--
--   0. Log only when using 'System.Distributed.Core.puts' and 'System.Distributed.Core.print'.
--   1. Log additionally 'System.Distributed.Core.trace' calls
--   2. Logs additionally traces using @'System.Distributed.Core.logStr' 2@.
--   3. Logs additionally every handler that gets run (system-defined trace)
--   4. Logs additionally every trigger that gets run (system-defined trace)
--   5. Traces also system-level information like events that aren't handled (system-defined trace)
--   6. Traces even more system-level information, like the bytes being sent through the network on an message (system-defined trace)
data SysConf' w = SysConf
  { verbosity :: w ::: Verbosity
      <#> "V" <!> "1" <?> "The system verbosity. Verbosity=3 traces every handler that gets run. Verbosity=4 traces also all triggers. Verbosity=5 traces also system-level information. Verbosity=6 traces even more system-level information, like the bytes being sent through the network on an message."
  , hostname  :: w ::: String <!> "127.0.0.1" <?> "The local hostname to bind this system too. Ideally, this should be the name other nodes will use to connect to it."
    -- ^ The IP that other nodes will use to connect to it is greatly preferred,
    -- since this allows network-protocol-tcp to re-use the connection.
  , port      :: w ::: Int    <#> "p" <?> "The port to run this system on."
  }
  deriving (Generic)

-- | Wrap a protocol which receives as an argument a record which is parseable from
-- the command line to pass to 'runProtos'.
--
-- To enable the configuration record to be parsed from the command line, it
-- must have an additional type argument (to insert cli-related information)
-- and @derive Generic@
--
-- === __Example__
--
-- @
-- data HyParViewConf arg =
--   HPVC
--     { maxSizeActiveView :: arg ::: Int
--         <?> "The max size of the active view"
--     , maxSizePassiveView :: arg ::: Int
--         <?> "The max size of the passive view"
--     , actRWL :: arg ::: Int
--         <?> "Active Random Walk Length"
--     , passRWL :: arg ::: Int
--         <?> "Passive Random Walk Length"
--     , shuffleTimer :: arg ::: Int
--         <!> "10000"
--         <?> "Time in millis to trigger the event for passive view management (SHUFFLE)"
--     , shuffleKa :: arg ::: Int
--         <?> "The number of nodes from the active view sent in a Shuffle message."
--     , shuffleKp :: arg ::: Int
--         <?> "The number of nodes from the passive view sent in a Shuffle message."
--     , shuffleTtl :: arg ::: Int
--         <?> "The ttl for shuffle messages."
--     , contactNode :: arg ::: Host
--         <#> "c"
--         <?> "The contact node"
--     }
--     deriving Generic
--
-- hyParView HPVC{..} = protocol @"HyParView" do
--   ...
--
-- main = runProtos [P hyParView]
-- @
data P = forall c name. ( GenericParseRecord (Rep (c Wrapped))
                        , Unwrappable c
                        , KnownSymbol name )
                        => P (c Unwrapped -> Protocol name)

data SomeP = forall name. SomeP (Protocol name)

--------------------------------------------------------------------------------
-- * Interpreter

-- | Run a t'Core' action directly given a system configuration.
--
-- The returned value, t'Sync', must be 'wait'ed for. Otherwise, the main
-- process will terminate regardless of the running threads.
runCore :: SysConf -> Core a -> IO Sync
runCore SysConf{..} c = do
  msgDecoders <- newMVar M.empty
  handlers    <- newIORef M.empty
  msgQueue    <- newChan
  mainSync    <- newSync

  (self, outgoingMsgs) <- initTcpManager verbosity hostname port msgDecoders msgQueue

  let coreData = CoreData { msgQueue, handlers, inProto = TopLevel
                          , verbosity, outgoingMsgs, self, msgDecoders
                          , mainSync
                          }

  _tid <- forkIO (worker `unCore` coreData)
  _    <- c `unCore` coreData
  return mainSync

-- | Interpret a 'System' into the t'Core' system runtime monad.
interpSystem :: System a -> Core ()
interpSystem sys = void $ iterM runF sys where

  runF :: SystemF (Core a) -> Core a
  runF = \case
    ProtocolBoundary i p n      -> interpProtocol i p                                      >>  n
    UponEvent x impl n          -> registerHandler x (\a ->
                                   traceStr 3 ("Handling " ++ show x) >> impl a)           >>  n
    TriggerEvent x e n          -> trace 4 ("Triggering " ++ show x)  >> queueEvent x e    >>  n
    SetupTimer tt evt timer n   -> trace' "Setting up " evt  >> startTimer tt evt timer    >>  n
    CancelTimer evt n           -> trace' "Handling " evt    >> stopTimer evt              >>  n
    ModifyState (Mutable a) b n -> liftIO (modifyIORef' a b)                               >>  n
      -- no need for atomic, handlers in the same protocol run atomically and
      -- never concurrently... though if you share state across protocols there could be race conditions.
      -- ToDo: Just use an MVar, or atomicModifyIORef'(if the documentation about its problem is incorrect), better to.
    MkNew i n                   -> liftIO (Mutable <$> newIORef i)                         >>= n
    GetState (Mutable a) n      -> liftIO (readIORef a)                                    >>= n
    GetRandom bnds n            -> liftIO (randomRIO bnds)                                 >>= n
    EscapeTheSystem io n        -> liftIO io                                               >>= n
    TraceStr v str n            -> trace v str                                             >>  n
    ExitProto                   -> exitProtocol
    GetSelf n                   -> n =<< asks self

  trace' kw x = trace 3 (kw ++ show x)

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
  (topLevelExec, _) <- newExecutor -- for top-level handlers

  forever $ do
    -- Waits for message
    Msg e t <- readChan cd.msgQueue

    handlers <- readIORef cd.handlers
    case M.lookup (EK e) handlers of
      Nothing ->
        {- message ignored -}
        traceInternal cd $
          "Ignored event " ++ show e -- ++ " given handlers " ++ show (M.toList handlers)
      Just hs -> do
        -- Run all handlers with the same message by passing them to the appropriate executor
        forM_ hs \(exectx, H h) -> do
          writeChan (case exectx of TopLevel -> topLevelExec; Scoped _ protoc _ -> protoc)
            (unsafeCoerce h t)

-- | Call 'wait' on the result of 'runCore' to halt the main thread until the
-- runtime decides the main thread can terminate.
-- Essentially, this means waiting forever since these distributed systems run
-- indefinitely.
wait :: Sync -> IO ()
wait (Sync m) = readMVar m

registerHandler :: ∀ t a. Event t -> (t -> System a) -> Core ()
registerHandler evt f = Core \cd -> do
  traceInternal cd $ "Registering handler for " ++ show evt
  let ek = EK evt
      h  = H $ (`unCore` cd) . interpSystem <$> f
  -- Multiple
  atomicModifyIORef' cd.handlers ((,()) . M.insertWith (<>) ek [(cd.inProto,h)])

  -- Handlers registered for Messages must also store the decoder function (see
  -- 'MsgDecoders') since messages come from the network with a type fingerprint only.
  case evt of
    Message{tyId} -> do {- matching on Message gives us a Binary instance for @t@ -}
      modifyMVar_ cd.msgDecoders (pure . M.insert tyId (MDEC @t Dict))
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
    registerCancelTimerH tid = atomicModifyIORef' cd.handlers ((,()) . M.insertWith (<>) (EK (StopTimer tr)) [(TopLevel, cancelTimerThread tid)])

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
interpProtocol :: String -> System a -> Core ()
interpProtocol name proto = do
  trace 5 $ "Interpreting protocol " ++ name ++ " in new thread"
  (protoExec, tid) <- liftIO newExecutor
  local (\cd -> case cd.inProto of
    TopLevel -> cd{inProto=Scoped name protoExec tid}
    -- TODO: Test nested protocols and see if they do the right thing.
    -- What about forM nested protocol with the same name? curious...
    Scoped parent_name _ _ -> cd{inProto=Scoped (parent_name ++ "-" ++ name) protoExec tid}
    ) $ interpSystem proto

-- | Kill a running protocol. But keep all other protocol threads running.
--
-- If the execution context is the top-level on exit, this notifies the
-- synchronisation variable which should terminate the program and kill all
-- remaining threads (if this ever happens to not be the case, we can think
-- about tracking all thread ids for killing).
exitProtocol :: Core a
exitProtocol = do
  asks inProto >>= \case
    TopLevel -> {- the end -} do
      -- notifying the top-level synchronisation variable means the whole
      -- process will terminate and therefore all other threads will be killed
      -- too.
      syn <- asks mainSync
      liftIO $ notifySync syn
      return undefined
    Scoped _n _c thread -> do
      liftIO $ killThread thread -- Stop running.
      return undefined

newExecutor :: IO (Chan (IO ()), ThreadId)
newExecutor = do
  ch <- newChan
  tid <- forkIO $ forever $ join (readChan ch) -- read an action from the channel and execute it forever
  return (ch, tid)

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
internalVerbosity = 5

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
  transport <- either (error . show) id <$>
    N.createTransport (N.defaultTCPAddr hostname (show port)) N.defaultTCPParameters
  endpoint <- either (error . show) id <$>
    N.newEndPoint transport

  -- Write outgoing messages to me, and I'll send them!
  outgoingMsgs <- newChan

  -- Keep some state on incoming and outgoing messages to flag the right events
  -- and reuse connections
  outConns <- newIORef M.empty

  -- Incoming side (receives connections)
  _ <- forkIO $ forever $ do
    event <- N.receive endpoint
    case event of
      N.Received _ [payload] -> do
        traceIO (internalVerbosity+1) ("Received message over the wire " ++ show payload) v "Network"
        let NetworkMessage{tyId, tyStr, msgPayload} = ndecode payload
        M.lookup tyId <$> readMVar decoders >>= \case
          Nothing ->
            netTrace $ "No handler/decoder found for " ++ show tyStr ++ ". Ignoring message..."
          Just (MDEC @et dict) -> withDict dict $
            case decodeOrFail $ fromStrict msgPayload of
              Left (full, _, err) ->
                netTrace $ "Failed to decode network message with error " ++ show err ++ ": " ++ show full
              Right (_, _, msg) ->
                -- Write the received message into the core message queue from
                -- where it will be dequeued and processed by the appropriate handlers
                writeChan coreMsgQ (Msg (Message @et tyId tyStr) msg)
      N.Received _ m ->
        netTrace $ "Unexpected message content: " ++ show m
      N.ConnectionOpened _ _ ep ->
        -- Flag in connection up
        writeChan coreMsgQ (Msg (ChannelEvt (typeRep @InConnUp)) InConnUp{from = Host ep})
      N.ReceivedMulticast{} ->
        netTrace "Ignoring received multicast"
      N.EndPointClosed ->
        netTrace "Endpoint closed?"
      N.ConnectionClosed i ->
        netTrace ("Connection closed cleanly " ++ show i ++ ", but closing a connection cleanly should only happen for outgoing connections to nodes which have closed the incoming connection already! Not flagging any channel event, please report a bug :)")
      N.ErrorEvent (N.TransportError (N.EventConnectionLost addr) _) -> do
        -- Flag that we lost an incoming connection
        writeChan coreMsgQ (Msg (ChannelEvt (typeRep @InConnDown)) InConnDown{from=Host addr})
        -- Drop an outgoing connection to this node if one exists:
        M.lookup (Host addr) <$> readIORef outConns >>= \case
          Nothing -> pure ()
          Just conn -> do
            N.close conn
            atomicModifyIORef' outConns ((,()) . M.delete (Host addr))
            writeChan coreMsgQ (Msg (ChannelEvt (typeRep @OutConnDown)) OutConnDown{to=Host addr})
      N.ErrorEvent e -> do
        netTrace ("Unexpected error event " ++ show e)

  -- Outgoing side (starts connections)
  _ <- forkIO $ forever $ do
    Msg (Message msgTyFP msgTyStr) msg <- readChan outgoingMsgs

    let
      sendMessage outConn = do
        let payload = nencode $ NetworkMessage msgTyFP msgTyStr (nencode msg)
        traceIO (internalVerbosity+1) ("Sending message over the wire " ++ show payload) v "Network"
        N.send outConn [payload]
          >>= \case
            Left e -> netTrace ("Unexpected error when sending message " ++ show e)
            Right () -> pure ()

    M.lookup msg.to <$> readIORef outConns >>= \case
      Just outConn -> sendMessage outConn
      Nothing ->
        N.connect endpoint msg.to.addr N.ReliableOrdered N.defaultConnectHints {-todo: set timeout in hints ? -}
          >>= \case
            Left err ->
              -- Flag out connection failed
              writeChan coreMsgQ (Msg (ChannelEvt (typeRep @OutConnFailed)) OutConnFailed{ err, to=msg.to })
            Right outConn -> do
              -- Store connection
              -- (non-atomic, this is the only thread modifying this ref)
              modifyIORef' outConns (M.insert msg.to outConn)
              -- Flag channel up
              writeChan coreMsgQ (Msg (ChannelEvt (typeRep @OutConnUp)) OutConnUp{to = msg.to})
              -- Send it!
              sendMessage outConn

  return (Host (N.address endpoint), outgoingMsgs)

-- ToDo: Implement some failure detection mechanism that detects InConnDown
-- besides disconnects (say, timeout)? I think we can automatically do this by
-- providing a connectionHint with a timeout, then the connection should
-- timeout automatically

data NetworkMessage = NetworkMessage { tyId :: Fingerprint, tyStr :: String, msgPayload :: ByteString } deriving (Generic, Binary)

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

-- | Either top-level execution context, or a protocol-scoped execution context.
--
-- In the case of a top-level context, we keep a synchronisation variable to
-- notify if the top-level context was terminated -- the main thread will
-- terminate after being notified in the main sync point and kill all to.
--
-- In the case of a scoped context, we keep the name of the protocol context (for
-- tracing), the scoped protocol key, and the thread id of the running protocol
-- to kill when the protocol is exited.
data ExecutorContext = TopLevel | Scoped String ProtocolKey ThreadId

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
  , mainSync :: Sync
  }

-- | We need a way of converting a bytestring we got from the net into a type
-- we can pass to the handler. We assign a fingerprint identifying the type to
-- a decoder function whose result we can "safely" unsafe coerce into the
-- argument expected by the handler for the message whose type has that same
-- fingerprint.
--
-- MsgDecoders use an MVar because I noticed something weird about atomicity
-- with multiple IORefs at the same time in the documentation of `modifyIORef`,
-- so to avoid problems just use MVar for these.
--
-- ToDo: Change to IORef and do atomicModifyIORef' if safe on multiple vars? or
-- just keep as MVar(and perhaps change others?)
type MsgDecoders = MVar (Map Fingerprint MsgDecoder)
data MsgDecoder = forall a. MDEC (Dict (HasField "to" a Host, Binary a))

-- | An event is its own key, but the type is preserved in the type-rep field only.
data EventKey = forall t. EK (Event t)

newtype Sync = Sync (MVar ())

notifySync :: Sync -> IO ()
notifySync (Sync s) = putMVar s ()

newSync :: IO Sync
newSync = Sync <$> newEmptyMVar

--------------------------------------------------------------------------------
-- * Instances

instance Show Handler where
  show _ = "<handler>"

instance Show ExecutorContext where
  show TopLevel{} = "Core"
  show (Scoped n _ _) = n

deriving instance Show EventKey

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

