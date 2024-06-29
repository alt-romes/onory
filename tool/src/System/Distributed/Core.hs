{-# LANGUAGE UnicodeSyntax, PatternSynonyms, DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-} -- typeFingerprint
-- | The core language for distributed systems design.
-- In contrast, the prelude module exports this module plus the kitchen sink
-- and a lot of overloaded operators which may not be suitable in the context
-- of using this embedded language as proper Haskell.
module System.Distributed.Core where

import GHC.Fingerprint
import GHC.Records
import Data.String
import Type.Reflection
import Data.Typeable (typeRepFingerprint)
import System.Random (Random)

import System.Distributed.Free

--------------------------------------------------------------------------------
-- * Main

request :: ∀ req. Typeable req => Name -> Event req
request name = Request name (typeRep @req)

indication :: ∀ ind. Typeable ind => Name -> Event ind
indication name = Indication name (typeRep @ind)

protocol :: Name -> Protocol -> System ()
protocol = protocolBoundary

self :: System Host
self = getSelf

new :: a -> System (Mutable a)
new = mkNew

upon :: Event n -> (n -> System a) -> System ()
upon = uponEvent

trigger :: Event n -> n -> System ()
trigger = triggerEvent

receive :: ∀ msg. HasField "to" msg Host => Typeable msg => Event msg
receive = Message (typeFingerprint @msg) (show (typeRep @msg))

-- | Send a message. The type of message sent must have a field "to".
send :: ∀ msg. HasField "to" msg Host => Typeable msg => Event msg
send = Message (typeFingerprint @msg) (show (typeRep @msg))

timer :: ∀ timer. HasField "time" timer Int => Typeable timer => Event timer
timer = Timer (typeRep @timer)

setup :: HasField "time" timer Int {- time field in milliseconds -}
      => TimerType timer -> Event timer -> timer -> System ()
setup = setupTimer

periodic :: HasField "repeat" timer Int {- repeat every n milliseconds -} => TimerType timer
periodic = PeriodicTimer

oneshot :: TimerType timer
oneshot  = OneShotTimer

cancel :: Event timer -> System ()
cancel = cancelTimer

random :: Random a => (a, a) -> System a
random = getRandom

logStr :: Int -> String -> System ()
logStr = traceStr

ok :: System ()
ok = pure ()

--------------------------------------------------------------------------------
-- * State manipulation

modify :: Mutable a -> (a -> a) -> System ()
modify = modifyState 

get :: Mutable a -> System a
get = getState 

pattern (:=) :: Mutable a -> a -> System ()
pattern (:=) <- _ {- what does this direction mean -}
  where (:=) ref x = modify ref (const x)

--------------------------------------------------------------------------------
-- * Logging

print :: Show a => a -> System ()
print = logStr 0 . show

puts :: String -> System ()
puts = logStr 0

trace :: String -> System ()
trace = logStr 1

--------------------------------------------------------------------------------
-- * Network

-- | Make a host from a String and an Int
host :: String -> Int -> Host
host hostname port = fromString $ hostname ++ ":" ++ show port

-- ** Channel events

outConnUp :: Event OutConnUp
outConnDown :: Event OutConnDown
outConnFailed :: Event OutConnFailed
inConnUp :: Event InConnUp
inConnDown :: Event InConnDown
outConnUp     = ChannelEvt (typeRep @OutConnUp)
outConnDown   = ChannelEvt (typeRep @OutConnDown)
outConnFailed = ChannelEvt (typeRep @OutConnFailed)
inConnUp      = ChannelEvt (typeRep @InConnUp)
inConnDown    = ChannelEvt (typeRep @InConnDown)

--------------------------------------------------------------------------------
-- * Escape hatch

doIO :: IO a -> System a
doIO = escapeTheSystem

typeFingerprint :: ∀ msg. Typeable msg => Fingerprint
typeFingerprint = typeRepFingerprint $ SomeTypeRep (typeRep @msg)

