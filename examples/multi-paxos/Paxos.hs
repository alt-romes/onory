{-# HLINT ignore "Redundant bracket" #-}

-- | Paxos as in Paxos Made Moderately Complex
-- https://courses.cs.washington.edu/courses/cse452/22wi/papers/van-renesse-altinbuken-paxos-made-moderately-complex.pdf
module Paxos where

import System.Distributed.Prelude

--------------------------------------------------------------------------------
-- Configuration

data PaxosConf s cli = PaxosConf
  { initialState   :: cli ::: s
  , initialLeaders :: cli ::: Set Host
  , slotWindow     :: cli ::: Int
  }
  deriving Generic

--------------------------------------------------------------------------------
-- Main

paxos :: Map String (st -> st) -- ^ Known operations
      -> PaxosConf st FromCli  -- ^ The configuration read from the command line interface
      -> Protocol "paxos"
paxos knownOps PaxosConf{..} = protocol @"paxos" do

  protocol @"replica" do -- A sub-protocol

    -- The replica’s copy of the application state, which we will treat as
    -- opaque. All replicas start with the same initial application state.
    app_state <- new initialState
    
    -- The index of the next slot in which the replica has not yet proposed any
    -- command, initially 1.
    slot_in   <- new (1 :: Int)

    -- The index of the next slot for which it needs to learn a decision before
    -- it can update its copy of the application state, equivalent to the
    -- state’s version number (i.e., number of updates) and initially 1.
    slot_out  <- new (1 :: Int)

    -- An initially empty set of requests that the replica has received and are
    -- not yet proposed or decided.
    requests  <- new Set

    -- An initially empty map from slots to proposals that are currently outstanding.
    proposals <- new Map

    -- Another set of proposals that are known to have been decided (also
    -- initially empty).
    decisions <- new Map

    -- The set of leaders in the current configuration. The leaders of the
    -- initial configuration are passed as an argument to the replica.
    leaders   <- new initialLeaders

    -- ToDo: Change to procedure syntax
    let
      isReconfig(op) = case op of
        StateOp{} -> False
        ReconfigOp{} -> True

      propose() = do
        while (slot_in < slot_out + slotWindow && size requests > (0::Int)) do

          -- Reconfigure
          maybe_reconf_decision <- lookup (slot_in - slotWindow) decisions
          case maybe_reconf_decision of
            Nothing -> ok
            Just (_, _, op) ->
              when (isReconfig(op)) do
                leaders := op.leaders

          when (slot_in `notin` decisions) do
            slot_in_copy <- copy(slot_in)
            random_request <- randomElem(requests)
            requests -= random_request
            proposals += (slot_in_copy, random_request)
            foreach leaders \leader_host -> do
              trigger send ProposeMsg{to=leader_host, slot=slot_in_copy, cmd=random_request}
          next_slot <- slot_in + (1::Int)
          slot_in := next_slot

      perform((k,cid,op)) = undefined



    upon commandRequest \cmdName -> do

      requests += cmdName
      call propose()


--------------------------------------------------------------------------------
-- * Interface

-- A command is a triple ⟨κ,cid,operation⟩, where κ1 is the identifier of the
-- client that issued the command and cid is a client-local unique command
-- identifier (e.g., a sequence number).
--
-- An operation is either a string referring to a specific an operation on the
-- state given in the known operations map, or a reconfiguration operation which carries a set of leaders.
type Cmd = (Int, Int, String)
data Op = StateOp { name :: String }
        | ReconfigOp { leaders :: Set Host }

commandRequest = request @Cmd "Command"

data ProposeMsg = ProposeMsg { to :: Host, slot :: Int, cmd :: Cmd } deriving (Generic, Binary)
data DecideMsg  = DecideMsg  { to :: Host, slot :: Int, cmd :: Cmd } deriving (Generic, Binary)

