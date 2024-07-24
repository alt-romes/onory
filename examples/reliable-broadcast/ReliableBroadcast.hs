{- |
Reliable Broadcast:
• RB1 (Validity): If a correct process i broadcasts message m, then i
    eventually delivers the message.
• RB2 (No Duplications): No message is delivered more than once.
• RB3 (No Creation): If a correct process j delivers a message m, then m was
    broadcast to j by some process i.
• RB4 (Agreement): If a message m is delivered by some correct process i, then
    m is eventually delivered by every correct process j.
-}
module ReliableBroadcast where

import System.Distributed.Prelude
import BestEffortBroadcast (bebReq, bebDeliver)

-- Choose default type for literal numbers
default (Int)

--------------------------------------------------------------------------------
-- Interface

rbReq = request @String "rbReq"
rbDeliver = indication @String "rbIndication"

reliableBroadcast NoConf = protocol @"rb" do

  uuidSupply <- new (0::Int)
  delivered  <- new Set

  let getUUID() = do
        next <- uuidSupply + 1
        uuidSupply := next
        return next

  upon rbReq \m -> do
    trigger rbDeliver m
    mid <- getUUID()
    delivered += mid
    trigger bebReq BebPayload{mid, msg=m}

  upon bebDeliver \BebPayload{mid, msg=m} -> do
    when (mid `notin` delivered) do
      delivered += mid
      trigger rbDeliver m
      trigger bebReq BebPayload{mid, msg=m}

--------------------------------------------------------------------------------
-- Data

data BebPayload = BebPayload {mid :: Int, msg :: String} deriving (Generic, Binary)
