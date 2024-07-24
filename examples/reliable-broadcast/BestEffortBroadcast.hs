{- |
Best Effort Broadcast:
• BEB1 (Best-Effort validity): For any two correct processes i and j, every
    message broadcasted by i is eventually delivered by j.
• BEB2: (No Duplication): No message is delivered more than once.
• BEB3: (No Creation): If a correct process j delivers a message m, then m was
    broadcast by some process i.

Usage: @bestEffortBroadcast (Proxy \@SomeMessagePayloadType)@
-}
module BestEffortBroadcast where

import Data.Typeable
import System.Distributed.Free
import System.Distributed.Prelude

-- Choose default type for literal numbers
default (Int)

-- Best effort broadcast has some additional signatures and arguments because
-- it is polymorphic on the payload of the message.

--------------------------------------------------------------------------------
-- Interface

bebReq, bebDeliver :: ∀ payload. (Typeable payload, Binary payload) => Event payload
bebReq     = request @payload "bebBroadcast"
bebDeliver = indication @payload "bebDeliver"

--------------------------------------------------------------------------------
-- Algorithm

bestEffortBroadcast :: ∀ payload. (Typeable payload, Binary payload)
                    => Proxy payload
                    -> BebConf FromCli
                    -> Protocol "beb"
bestEffortBroadcast _ BebConf{..} = protocol @"beb" do

  upon (bebReq @payload) \m -> do
    foreach processes \p -> do
      trigger send BebMsg{to = p, msg = m}

  upon (receive @(BebMsg payload)) \BebMsg{msg} -> do
    trigger bebDeliver msg

--------------------------------------------------------------------------------
-- Messages

data BebMsg a = BebMsg { to :: Host, msg :: a } deriving (Generic, Binary)

--------------------------------------------------------------------------------
-- Conf

newtype BebConf cli = BebConf
  { processes :: cli ::: Set Host
  }
  deriving Generic
