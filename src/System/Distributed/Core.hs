{-# LANGUAGE UnicodeSyntax, PatternSynonyms, DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-} -- typeFingerprint
-- | The core embeded language for distributed systems design.
--
-- In contrast, the prelude module exports this module plus the kitchen sink
-- and a lot of overloaded operators which may not be suitable in the context
-- of using this embedded language as proper Haskell.
module System.Distributed.Core where

import GHC.Fingerprint
import GHC.Records
import GHC.TypeLits
import Data.Binary
import Data.Proxy
import Data.String
import Type.Reflection
import Data.Typeable (typeRepFingerprint)
import System.Random (Random)

import System.Distributed.Free

--------------------------------------------------------------------------------
-- * Core

-- | Get the t'Host' that this system is running on.
--
-- @
-- do
--  myself <- 'self'
--  'System.Distributed.Core.print' myself -- e.g. "127.0.0.1:25565"
-- @
self :: System Host
self = getSelf

-- | Register a handler to run upon the given event being triggered.
--
-- The first argument is the event upon which we'll execute the handler.
-- The second argument is the handler.
--
-- Events are typically 'request', 'indication', 'receive', and 'timer':
--
--   * Request and indications events are declared at the top-level since they
--   are triggered/handled cross protocol.
--
--   * Message ('receive'/'send') and Timer ('timer') events should be used
--   per-protocol and are disambiguated solely by the type of message/timer being
--   handled/triggered.
--
-- == Indications
--
-- Indications are triggered by some protocol to notify that some event
-- specific to that protocol has occurred. Other protocols can subscribe to
-- that indication by registering a handler for it. 
--
-- @
-- neighbourUp = 'indication' \@Host "neighbourUp"
--
-- proto1 = 'protocol' \@"p1" do
--   'trigger' neighbourUp host1
--
-- proto2 = 'protocol' \@"p2" do
--   'upon' neighbourUp \\host -> do
--     'System.Distributed.Core.print' host
-- @
--
-- == Requests
--
-- Requests work very much like indications, but conceptually reflect a
-- protocol /requesting/ another to do something, rather than indicating
-- something has happened.
-- Typically, in protocol stacks, requests flow in one direction and
-- indications in the other:
--
-- @
--            ┌──────┐        
--            │Proto1│        
--            └─▲───┬┘        
--              │   │         
--  indication1 │   │ request1
--              │   │         
--            ┌─┴───▼┐        
--            │Proto2│        
--            └▲───┬─┘        
--             │   │          
-- indication2 │   │ request2 
--             │   │          
--            ┌┴───▼─┐        
--            │Proto3│        
--            └──────┘        
-- @
--
-- Here's an example:
--
-- @
-- broadcastRequest = 'request' \@String "broadcast"
--
-- proto2 = 'protocol' \@"p2" do
--   'trigger' broadcastRequest "Please broadcast me!"
--
-- proto1 = 'protocol' \@"p1" do
--   'upon' broadcastRequest \\msg -> do
--      -- Send message to all known nodes
--      ...
-- @
--
-- == Messages
--
-- Messages are sent between the same protocol executing on different
-- processes.
--
-- A message is identified by the datatype specifying the data carried in that message.
-- Every message datatype must contain a field @to :: Host@ identifying the
-- Host executing the same protocol to which the message will be sent. Using a
-- made-up ping-pong system which forwards pings to other hosts as an example:
--
-- @
-- ┌──┐     ┌──┐         ┌──┐
-- │P1│     │P2│         │P3│
-- └┬─┘     └┬─┘         └┬─┘
--  │        │            │  
--  │ping msg│            │  
--  │───────>│            │  
--  │        │            │  
--  │        │fwd ping msg│  
--  │        │───────────>│  
--  │        │            │  
--  │pong msg│            │  
--  │<───────│            │  
--  │        │            │  
--  │    long pong msg    │  
--  │<────────────────────│  
-- ┌┴─┐     ┌┴─┐         ┌┴─┐
-- │P1│     │P2│         │P3│
-- └──┘     └──┘         └──┘
-- @
--
-- For instance, a Ping message and Pong message would have two fields, a @to@
-- field (required by the implementation) and a @from@ field (user decided), to
-- provide the receiving end with the contact to reply to:
--
-- @
-- data PingMessage = PingMessage { to :: t'Host', from :: t'Host' } deriving (Generic, 'Binary')
-- data PongMessage = PongMessage { to :: t'Host', from :: t'Host' } deriving (Generic, 'Binary')
-- @
--
-- Note that message datatypes must instance 'Binary', specifying how the data
-- will be serialised before being sent over the wire to other processes.  This
-- can be done automatically by writing @deriving (Generic, Binary)@ in front
-- of the datatype declaration.
--
-- Handlers for message events will receive as an argument the Message that is
-- being handled.
--
-- @
-- myself <- 'self'
-- 'upon' 'receive' \\PingMessage{from = sender} -> do
--   'trigger' 'send' PongMessage{to = sender, from = myself}
-- @
--
-- When matching on the @PingMessage@, we bind the value of the @from@ field
-- of the message to the @sender@ variable (and ignore the @to@ field, since
-- that would be ourselves who received the message). When constructing a
-- @PongMessage@, we use the same curly bracket syntax to provide values for
-- each field -- here we can't omit fields since we are constructing the
-- message, not matching on it. We set the field @to@ of the @PongMessage@ to
-- the @sender@ (which we got from the received message), and the @from@ field
-- of the message to ourselves (gotten from calling 'self').
--
-- == Timers
--
-- Timers are 'setup' as 'periodic' or 'oneshot' timers. When the timer fires,
-- a new timer event is created. We can register handlers using 'upon' to
-- handle timer events.  The timer event will carry a value of the timer
-- datatype that was setup. It typically only contains information about the
-- timer delay, but it can also hold user-defined information.
-- 
-- Like messages, the timers are identified by associated the timer datatype.
-- The timer datatype must always have a @time :: Int@ field specifying the
-- milliseconds delay for the timer to fire. A repeating timer must
-- additionally have a @repeat :: Int@ for the periodic repeat period.
--
-- @
-- data MyTimer = MyTimer { time :: Int }
--
-- sys = do
--   'setup' 'oneshot' 'timer' MyTimer{time=100}
--
--   'upon' 'timer' \\MyTimer{} -> do
--     'puts' "My timer fired!"
-- @
--
-- Note how we match on @MyTimer{}@ because we are defining a handler for
-- @MyTimer@ events, but we don't care about any of its fields (hence the
-- @{}@). If there were a field whose value we'd like to bind, we'd use the
-- same syntax that we did to bind Message fields.
--
-- == Channel Events
--
-- There are 5 known channel events which you can register handlers for using 'upon'.
-- The known channel events are 'outConnUp', 'outConnDown', 'outConnFailed', 'inConnUp', 'inConnDown'.
--
-- @
-- 'upon' 'outConnUp' \\ v'OutConnUp'{to = outto} -> do
--   'puts' ("Successfully established connection to " ++ show outto)
-- @
--
upon :: Event n -> (n -> System a) -> System ()
upon = uponEvent

-- | Trigger an event using with the appropriate value for that event.
-- Similarly to 'upon', let's describe how each type of event would be triggered with an example:
--
-- == Indications
--
-- Indications must be declared using 'indication', given a name and a type.
-- The 'trigger' keyword receives the declared indication event and a value of the type associated with that indication.
--
-- @
-- ready = 'indication' "ready" @Int
-- sys = do
--   'trigger' ready (12345 :: Int)
-- @
--
-- == Requests
--
-- Requests work like indications, even though they represent different concepts (see an explanation in 'upon').
--
-- @
-- requestBroadcast = 'request' "broadcast" @String
-- sys = do
--   'trigger' requestBroadcast "This msg should be broadcasted by a protocol which handles this event"
-- @
--
-- == Messages
--
-- A message event can be triggered using the 'send' keyword and a value of the
-- datatype of the message you want to send. See more on messages in 'upon'.
--
-- @
-- data PingMessage = PingMessage { to :: Host, from :: Host } deriving (Generic, Binary)
-- sys = do
--   myself <- 'self'
--   'trigger' 'send' PingMessage{ to = 'host' "127.0.0.1" 25566, from = myself }
-- @
--
-- == Timers
--
-- A timer event can only be setup rather than triggered directly. When the
-- timer fires, it will trigger the event. See 'setup'.
--
-- == Channel Events
-- 
-- Channel events are triggered automatically by the runtime system. For an
-- explanation on handling channel events see 'upon'.
trigger :: Event n -> n -> System ()
trigger = triggerEvent

-- | Create a new mutable reference with a given initial value
-- 
-- @
-- int_ref <- new (0::Int)
-- @
--
-- See also 'modify', 'System.Distributed.Core.get', and ':='.
new :: a -> System (Mutable a)
new = mkNew

-- | Setup a 'oneshot' or 'periodic' timer (see the corresponding documentation for details on each type of timer).
--
-- Each timer has its separate timer datatype. All timer datatypes need a @time
-- :: Int@ field specifying the delay until the timer first fires.
--
-- @
-- data MyTimer = MyTimer {time :: Int}
-- sys = do
--    'setup' 'oneshot' 'timer' MyTimer{time=100}
-- @
--
-- Setting up a timer means to start the timer and trigger a timer event with
-- the timer (data) that was setup. When a timer event fires, it can be handled by a timer handler:
--
-- @
-- data MyTimer = MyTimer {time :: Int}
-- sys = do
--    'setup' 'oneshot' 'timer' MyTimer{time=100}
--
--    'upon' 'timer' \\MyTimer{} -> do
--      'puts' "MyTimer fired!"
-- @
--
-- For 'periodic' timers, the timer datatype must also have a @repeat :: Int@
-- field specifying the periodic trigger delay that will be repeatedly
-- triggered forever or until 'cancel'ed. As all other datatypes, you can also
-- embed additional information in the timer which will be made available to
-- handlers for that timer.
--
-- @
-- data MyRTimer = MyRTimer {time :: Int, mystr :: Int}
-- sys = do
--    if condition then
--      'setup' 'oneshot' 'timer' MyRTimer{time=200, mystr="Triggered because condition was true"}
--    else
--      'ok'
--
--    'upon' 'timer' \\MyRTimer{mystr = reason_str} -> do
--      'puts' "Handled timer MyRTimer:"
--      'puts' reason_str
-- @
--
-- A timer with a @repeat :: Int@ field can be setup both as a one-shot timer and periodic timer.
--
setup :: HasField "time" timer Int {- time field in milliseconds -}
      => TimerType timer -> Event timer -> timer -> System ()
setup = setupTimer

-- | Cancel a timer event.
--
-- Cancelling a timer will stop it from firing completely after it is
-- cancelled, regardless of whether the canceled timer is 'periodic' or
-- 'oneshot'.
--
-- @
-- data MyRepeatTimer = MyRepeatTimer{ time :: Int, repeat :: Int}
-- data MyOneShotTimer = MyOneShotTimer{ time :: Int }
--
-- -- This system will print out "Repeated!" only twice
-- sys = do
--   -- Will trigger every 20ms after first trigger in 40ms
--   'setup' 'periodic' 'timer' MyRepeatTimer{time=40, repeat=20}
--   -- Will trigger in 70ms
--   'setup' 'oneshot' 'timer' MyOneShotTimer{time=70}
--
--   'upon' 'timer' \\MyRepeatTimer{} -> do
--      'puts' "Repeated!"
--
--   'upon' 'timer' \\MyOneShotTimer{} -> do
--      'cancel' \@MyRepeatTimer 'timer'
-- @
cancel :: Event timer -> System ()
cancel = cancelTimer

-- | Get a random value within the given bound.
-- The random value can be inclusively one of the bounds.
--
-- @
-- random_int <- 'random'(0,255)
-- 'System.Distributed.Core.print' random_int -- prints value between 0 and 255 inclusive
-- @
--
-- You may also be looking for 'System.Distributed.Prelude.randomElem', 'System.Distributed.Prelude.randomElemWith', or 'System.Distributed.Prelude.randomSubset' from 'System.Distributed.Prelude'.
random :: Random a => (a, a) -> System a
random = getRandom

-- | Log a string with the given verbosity
--
-- @
-- 'logStr' 3 "This will only get logged if --verbosity=3 or higher"
-- @
logStr :: Int -> String -> System ()
logStr = traceStr

-- | Declare a protocol boundary around the given system.
-- All protocols (systems within a protocol boundary) within the process will
-- be run concurrently. Communication accross protocols within the same
-- process is done with requests and indications.
--
-- @
-- hyParView :: HyParViewConf -> 'Protocol' \"HyParView\"
-- hyParView = 'protocol' \@\"HyParView\" do
--   'puts' "Starting HyParView..."
--   myself <- 'self'
--   'puts' ("I am " ++ show myself)
--   ... -- rest of the system
-- @
--
-- It is often desireable to delegate joining the protocols into a system to
-- the interpreter, since the interpreter will do additional tasks for each
-- protocol, such as creating command line interface options for each
-- protocol's configuration.
--
-- @
-- proto1 :: Conf1 -> 'Protocol' "p1"
-- proto1 conf = 'protocol' \@"p1" do ...
--
-- proto2 :: Conf2 -> 'Protocol' "p2"
-- proto2 conf = 'protocol' \@"p2" do ...
--
-- main = 'System.Distributed.Interpret.runProtos' ['System.Distributed.Interpret.P' proto1, 'System.Distributed.Interpret.P' proto2]
-- @
--
-- A protocol still forms a 'System', so it can also be manually put together
-- with other protocols for the same "full" System that the process will run
-- (where each "sub-system" within a protocol boundary will be run concurrently).
--
-- @
-- proto1 :: 'Protocol' "p1"
-- proto2 :: 'Protocol' "p2"
-- 
-- fullSystem :: 'System' ()
-- fullSystem = do
--   proto1
--   proto2
--
-- main = 'System.Distributed.Interpret.runSystem' v'System.Distributed.Interpret.SysConf' { verbosity = 1
--                         , hostname="127.0.0.1"
--                         , port=25001 } fullSystem
-- @
protocol :: ∀ name a. KnownSymbol name => System a -> Protocol name
protocol s = protocolBoundary (symbolVal (Proxy @name)) s >> return (Proxy @name)

-- | Terminate a protocol. Note that this will only exit the running protocol
-- thread, and all other protocols in the system will keep running.
--
-- NB: No instructions after exit get run.
exit :: System a
exit = exitProto

-- | A system primitive that does absolutely nothing,
-- but can be useful to fill in certain expressions.
--
-- Can be thought of something like @pass@ in Python
--
-- @
-- if some_condition then
--   'puts' "Executing because condition was true!"
--   do_something
-- else
--   'ok'
-- @
ok :: System ()
ok = pure ()

--------------------------------------------------------------------------------
-- * Events

-- | A message event for sending messages. A message event is fully defined by
-- its associated datatype @msg@. The datatype associated to the message event
-- sent must have a field "to".
-- 
-- See the example under 'trigger' for messages.
send :: ∀ msg. (HasField "to" msg Host, Binary msg) => Typeable msg => Event msg
send = Message (typeFingerprint @msg) (show (typeRep @msg))

-- | A message event for receiving messages. A message event is fully defined by
-- its associated datatype @msg@.
--
-- See the example under 'upon' for messages.
receive :: ∀ msg. (HasField "to" msg Host, Binary msg) => Typeable msg => Event msg
receive = Message (typeFingerprint @msg) (show (typeRep @msg))

-- | Make a request event from a name and associated content type.
-- See 'trigger' and 'upon' for examples on how to use 'request'.
request :: ∀ req. Typeable req => String -> Event req
request name = Request name (typeRep @req)

-- | Make an indication event from a name and associated content type.
-- See 'trigger' and 'upon' for examples on how to use 'indication'.
indication :: ∀ ind. Typeable ind => String -> Event ind
indication name = Indication name (typeRep @ind)

-- | A timer event identified by the specific @timer@ datatype, which can be
-- 'setup' or handled with 'upon'. See the 'setup' documentation for examples.
timer :: ∀ timer. HasField "time" timer Int => Typeable timer => Event timer
timer = Timer (typeRep @timer)

--------------------------------------------------------------------------------
-- * Timer Types

-- | A repeated timer type. For use in conjunction with 'setup' and 'timer'.
--
-- A repeating timer will fire the first time after X milliseconds, and repeat
-- forever every Y milliseconds (if not 'cancel'ed before).
-- The delay X to fire the timer the first t ime is given by the @time@ field
-- in the timer datatype. The repeat delay Y is given by the 'repeat' field in
-- the timer datatype.
--
-- @
-- -- Must have the field @time :: Int@ for first delay and @repeat :: Int@ for repeat delay
-- data MyTimer = MyTimer {time :: Int, repeat :: Int}
--
-- sys = do
--   'setup' 'periodic' 'timer' MyTimer{time=100, repeat=200}
-- @
periodic :: HasField "repeat" timer Int {- repeat every n milliseconds -} => TimerType timer
periodic = PeriodicTimer

-- | A one-shot timer type. For use in conjunction with 'setup' and 'timer'.
--
-- A one-shot timer will fire exactly once (if not 'cancel'ed before).
-- The delay to fire the timer is given by the @time@ field in the timer datatype
--
-- @
-- -- Must have the field @time :: Int@ for delay in milliseconds
-- data MyTimer = MyTimer {time :: Int}
--
-- sys = do
--   'setup' 'oneshot' 'timer' MyTimer{time=100}
-- @
oneshot :: TimerType timer
oneshot  = OneShotTimer

--------------------------------------------------------------------------------
-- * State manipulation (slightly lower-level)

-- | Modify the value of a mutable reference
--
-- @
-- int_ref <- 'new' (0::Int)
-- 'modify' int_ref (\x -> x ** 2)
-- @
modify :: Mutable a -> (a -> a) -> System ()
modify = modifyState

-- | Get the value of a mutable reference
--
-- @
-- knownNodes <- 'new' Set
-- knownNodes += some_host
-- ...
--
-- 'upon' ... \... -> do
--   kn <- 'System.Distributed.Core.get' knownNodes
--   'System.Distributed.Core.print' kn
-- @
get :: Mutable a -> System a
get = getState 

-- | Assign a value to a mutable reference
--
-- @
-- int_ref <- new (0::Int)
-- if condition then
--   int_ref := 10
-- else
--   int_ref := 15
-- @
pattern (:=) :: Mutable a -> a -> System ()
pattern (:=) <- _ {- what does this direction mean -}
  where (:=) ref x = modify ref (const x)

--------------------------------------------------------------------------------
-- * Logging

-- | Transform the given argument into a string and then print it onto the standard output
--
-- @
-- i <- 'random'(0,255::Int) + (1::Int)
-- 'System.Distributed.Core.print' i
-- @
print :: Show a => a -> System ()

-- | Output the given string to the standard output
puts :: String -> System ()

-- | Output the given string to the standard output if --verbosity=1 or higher.
trace :: String -> System ()

print = logStr 0 . show
puts = logStr 0
trace = logStr 1

--------------------------------------------------------------------------------
-- * Network

-- ** Channel events

-- | A known channel event triggered when an outgoing connection is successfully established
outConnUp :: Event OutConnUp
-- | A known channel event triggered when an established outgoing connection is dropped
outConnDown :: Event OutConnDown
-- | A known channel event triggered when an outgoing connection is fails to be established
outConnFailed :: Event OutConnFailed
-- | A known channel event triggered when an incoming connection is established
inConnUp :: Event InConnUp
-- | A known channel event triggered when an established incoming connection is dropped
inConnDown :: Event InConnDown
outConnUp     = ChannelEvt (typeRep @OutConnUp)
outConnDown   = ChannelEvt (typeRep @OutConnDown)
outConnFailed = ChannelEvt (typeRep @OutConnFailed)
inConnUp      = ChannelEvt (typeRep @InConnUp)
inConnDown    = ChannelEvt (typeRep @InConnDown)

-- ** Host

-- | Manually make a host from a String and an Int.
host :: String -> Int -> Host
host hostname port = fromString $ hostname ++ ":" ++ show port

--------------------------------------------------------------------------------
-- * Escape hatch

-- | Lift an IO action into a System. You likely don't need this unless using
-- onory as a Haskell library rather than embedded language.
doIO :: IO a -> System a
doIO = escapeTheSystem

-- | An internal function for type magic
typeFingerprint :: ∀ msg. Typeable msg => Fingerprint
typeFingerprint = typeRepFingerprint $ SomeTypeRep (typeRep @msg)

