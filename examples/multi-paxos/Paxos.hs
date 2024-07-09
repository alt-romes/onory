{-# HLINT ignore "Redundant bracket" #-}

-- | Paxos as in Paxos Made Moderately Complex
-- https://courses.cs.washington.edu/courses/cse452/22wi/papers/van-renesse-altinbuken-paxos-made-moderately-complex.pdf
module Paxos where

import System.Distributed.Prelude

default (Int) -- Needed, specify what the default type for number literals should be (Int, Float, etc...).

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

paxos :: (FromCli ::: st) ~ st
      => Map String (st -> (st, String)) -- ^ Known operations
      -> PaxosConf st FromCli  -- ^ The configuration read from the command line interface
      -> Protocol "paxos"
paxos knownOps PaxosConf{..} = protocol @"paxos" do
  puts "Starting paxos..."

  protocol @"replica" do -- A sub-protocol

    -- The replica’s copy of the application state, which we will treat as
    -- opaque. All replicas start with the same initial application state.
    app_state <- new initialState
    
    -- The index of the next slot in which the replica has not yet proposed any
    -- command, initially 1.
    slot_in   <- new 1

    -- The index of the next slot for which it needs to learn a decision before
    -- it can update its copy of the application state, equivalent to the
    -- state’s version number (i.e., number of updates) and initially 1.
    slot_out  <- new 1

    -- An initially empty set of requests that the replica has received and are
    -- not yet proposed or decided.
    requests  <- new (Set @Cmd)

    -- An initially empty map from slots to proposals that are currently outstanding.
    proposals <- new (Map @Int @Cmd)

    -- Another set of proposals that are known to have been decided (also
    -- initially empty).
    decisions <- new (Map @Int @Cmd)

    -- The set of leaders in the current configuration. The leaders of the
    -- initial configuration are passed as an argument to the replica.
    leaders   <- new initialLeaders

    -- ToDo: Change to procedure syntax
    let
      propose() = do
        while (slot_in < slot_out + slotWindow && size requests > 0) do

          -- Reconfigure
          reconf_decision <- lookup (slot_in - slotWindow) decisions
          when (reconf_decision.exists) do
            let (_, _, op) = reconf_decision.value
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

      perform((k::Int,cid::Int,op)) = do
        older_decision <- decisions `randomElemWith'` (\(slot,(k'::Int,cid'::Int,op')) ->
            slot < slot_out && k == k' && cid == cid' && op == op')
        if (isReconfig(op) || older_decision.exists) then do
          slot_in_val <- slot_in + (1::Int)
          slot_in := slot_in_val
        else do
          operation  <- lookup op.name knownOps
          state_copy <- copy(app_state)
          let (next, result) = operation.value(state_copy)
          app_state := next
          next_slot <- slot_in + (1::Int)
          slot_out := next_slot
          trigger responseIndication result

    upon commandRequest \cmdName -> do
      requests += cmdName
      propose()

    upon receive \DecideMsg{slot, cmd} -> do
      decisions += (slot, cmd)
      while (decisions `contains` slot_out) do
        c' <- proposals ! slot_out
        c'' <- lookup slot_out proposals
        when (c''.exists) do
          slot_out_val <- get(slot_out)
          proposals -= slot_out_val
          when (c' != c''.value) do
            requests += c''.value
        perform(c')

      -- Finally, propose again
      propose()

  protocol @"leader" do

    ok

  protocol @"acceptor" do

    ballot_num <- new (-1::Int)
    accepted   <- new (Set @PValue)
    ok

    -- upon receive \P1A{} -> do
    --   ok

    -- upon receive \P2A{} -> do
    --   ok

--------------------------------------------------------------------------------
-- * Replica Interface

-- A command is a triple ⟨κ,cid,operation⟩, where κ1 is the identifier of the
-- client that issued the command and cid is a client-local unique command
-- identifier (e.g., a sequence number).
--
-- An operation is either a string referring to a specific an operation on the
-- state given in the known operations map, or a reconfiguration operation which carries a set of leaders.
type Cmd = (Int, Int, Op)
data Op = StateOp { name :: String }
        | ReconfigOp { leaders :: Set Host }
        deriving (Eq, Ord, Generic, Binary)

isReconfig(StateOp{}) = False
isReconfig(ReconfigOp{}) = True

commandRequest = request @Cmd "Command"
responseIndication = request @String "ClientResponse"

data ProposeMsg = ProposeMsg { to :: Host, slot :: Int, cmd :: Cmd } deriving (Generic, Binary)
data DecideMsg  = DecideMsg  { to :: Host, slot :: Int, cmd :: Cmd } deriving (Generic, Binary)

--------------------------------------------------------------------------------
-- * Acceptor Interface

-- | Let a pvalue be a triple consisting of a ballot number, a slot number,
-- and a command
type PValue = (Int, Int, Cmd)
