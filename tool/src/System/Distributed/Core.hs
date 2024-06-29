{-# LANGUAGE UnicodeSyntax, PatternSynonyms, DataKinds #-}
-- | The core language for distributed systems design.
-- In contrast, the prelude module exports this module plus the kitchen sink
-- and a lot of overloaded operators which may not be suitable in the context
-- of using this embedded language as proper Haskell.
module System.Distributed.Core
  ( module System.Distributed.Core
  )
  where

import GHC.Records
import Type.Reflection
import System.Random (Random)

import System.Distributed.Free

--------------------------------------------------------------------------------
-- * Main

request :: ∀ req. Typeable req => Name -> Event req
request name = Request name (typeRep @req)

indication :: ∀ ind. Typeable ind => Name -> Event ind
indication name = Indication name (typeRep @ind)

self :: System Host
self = getSelf

new :: a -> System (Mutable a)
new = mkNew

upon :: Event n -> (n -> System a) -> System ()
upon = uponEvent

trigger :: Event n -> n -> System ()
trigger = triggerEvent

receive :: ∀ msg. Typeable msg => Event msg
receive = Message (typeRep @msg)

-- | Send a message. The type of message sent must have
send :: ∀ msg. Typeable msg => Event msg
send = Message (typeRep @msg)

timer :: ∀ timer. Typeable timer => Event timer
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
-- * Escape hatch

doIO :: IO a -> System a
doIO = escapeTheSystem
