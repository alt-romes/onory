{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Redundant return" #-}

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
  , knownAcceptors :: cli ::: Set Host
  , knownReplicas  :: cli ::: Set Host
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
  myself <- self

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
          next_slot <- slot_in + 1
          slot_in := next_slot

      perform((k,cid,op)) = do
        older_decision <- decisions `randomElemWith'` (\(slot,(k',cid',op')) ->
            slot < slot_out && k == k' && cid == cid' && op == op')
        if (isReconfig(op) || older_decision.exists) then do
          slot_in_val <- slot_in + 1
          slot_in := slot_in_val
        else do
          operation  <- lookup op.name knownOps
          state_copy <- copy(app_state)
          let (next, result) = operation.value(state_copy)
          app_state := next
          next_slot <- slot_in + 1
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

  protocol @"acceptor" do

    ballot_num <- new (-1, undefined)
    accepted   <- new (Set @PValue)

    upon receive \P1AMsg{leader, ballot} -> do
      when (ballot > ballot_num) do
        ballot_num := ballot
      accpt <- get(accepted)
      bltn  <- get(ballot_num)
      trigger send P1BMsg{to=leader, from=myself, ballot=bltn, accepted=accpt}

    upon receive \P2AMsg{leader, pv=(b, s, c)} -> do
      when (b == ballot_num) do
        accepted += (b,s,c)
      bn <- get(ballot_num)
      trigger send P2BMsg{to=leader, from=myself, ballot=bn}

  protocol @"leader" do

    myself     <- self
    ballot_num <- new (0, myself)
    active     <- new false
    proposals  <- new (Map @Int @Cmd)

    bn <- get(ballot_num)
    scout myself knownAcceptors bn

    upon receive \ProposeMsg{slot=s, cmd=c} -> do
      when (s `notin` proposals) do
        proposals += (s,c)
        when active do
          bn <- get(ballot_num)
          commander myself knownAcceptors knownReplicas (bn, s, c)

    upon receive \AdoptedMsg{ballot=bn, pvalues=pvals} -> do
      props <- get(proposals)
      pmx   <- pmax(pvals)
      new_props <- props <| pmx
      proposals := new_props
      foreach proposals \(s,c) -> do
        commander myself knownAcceptors knownReplicas (bn, s, c)
      active := true

    upon receive \PreemptedMsg{ballot=(r',l')} -> do
      when ((r', l') > ballot_num) do
        active := false
        r'' <- r' + 1
        ballot_num := (r'', myself)
        bn <- get(ballot_num)
        scout myself knownAcceptors bn

commander leader acceptors replicas (b,s,c) = protocol @"commander" do
  myself <- self
  wait_for <- new acceptors

  foreach acceptors \a -> do
    trigger send P2AMsg{to=a, leader=myself, pv=(b,s,c)}

  upon receive \P2BMsg{from=a, ballot=b'} -> do
    if b' == b then do
      wait_for -= a
      when (size wait_for < (size acceptors `div` 2)) do
        foreach replicas \p -> do
          trigger send DecideMsg{to=p, slot=s, cmd=c}
        exit
    else do
      trigger send PreemptedMsg{to=leader, ballot=b'}
      exit

scout leader acceptors b = protocol @"scout" do
  myself   <- self
  wait_for <- new acceptors
  pvalues  <- new (Set @PValue)
  foreach acceptors \a -> do
    trigger send P1AMsg{to=a, leader=myself, ballot=b}

  upon receive \P1BMsg{from=a, ballot=b', accepted=r} -> do
    if b' == b then do
      new_pvs <- pvalues `union` r
      pvalues := new_pvs
      wait_for -= a
      when (size wait_for < (size acceptors `div` 2)) do
        pvs <- get(pvalues)
        trigger send AdoptedMsg{to=leader, ballot=b, pvalues=pvs}
        exit
    else do
      trigger send PreemptedMsg{to=leader, ballot=b'}
      exit

(<|) props pm = pm `union` props -- left biased union implements <|

pmax pvs = do
  -- highest-ballot-per-slot
  slot_ballots <- new (Map @Int @(BN, Cmd))

  foreach pvs \(b, s, c) -> do
    r <- lookup s slot_ballots 
    if r.exists then do
      let (b', _c') = r.value
      when (b > b') do
        slot_ballots += (s, (b,c)) -- overwrites
    else do
      slot_ballots += (s, (b,c))
      ok

  result <- new (Map @Int @Cmd)
  foreach slot_ballots \(s, (_,c)) -> do
    result += (s,c)

  res <- get(result)
  return res

--------------------------------------------------------------------------------
-- * Interface

-- A command is a triple ⟨κ,cid,operation⟩, where κ1 is the identifier of the
-- client that issued the command and cid is a client-local unique command
-- identifier (e.g., a sequence number).
--
-- An operation is either a string referring to a specific an operation on the
-- state given in the known operations map, or a reconfiguration operation which carries a set of leaders.
type Cmd = (Host, Int, Op)
data Op = StateOp { name :: String }
        | ReconfigOp { leaders :: Set Host }
        deriving (Eq, Ord, Generic, Binary)

isReconfig(StateOp{}) = False
isReconfig(ReconfigOp{}) = True

commandRequest = request @Cmd "Command"
responseIndication = request @String "ClientResponse"

data ProposeMsg = ProposeMsg { to :: Host, slot :: Int, cmd :: Cmd } deriving (Generic, Binary)
data DecideMsg  = DecideMsg  { to :: Host, slot :: Int, cmd :: Cmd } deriving (Generic, Binary)

-- | Let a pvalue be a triple consisting of a ballot number, a slot number,
-- and a command
type PValue = (BN, Int, Cmd)

-- | Ballot number
type BN = (Int, Host)

data P1AMsg = P1AMsg { to :: Host, leader :: Host, ballot :: BN } deriving (Generic, Binary)
data P1BMsg = P1BMsg { to :: Host, from   :: Host, ballot :: BN, accepted :: Set PValue } deriving (Generic, Binary)

data P2AMsg = P2AMsg { to :: Host, leader :: Host, pv :: PValue } deriving (Generic, Binary)
data P2BMsg = P2BMsg { to :: Host, from   :: Host, ballot :: BN } deriving (Generic, Binary)

data PreemptedMsg = PreemptedMsg { to :: Host, ballot :: BN } deriving (Generic, Binary)
data AdoptedMsg = AdoptedMsg { to :: Host, ballot :: BN, pvalues :: Set PValue } deriving (Generic, Binary)

