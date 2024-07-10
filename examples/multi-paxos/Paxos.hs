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

data RandomOp = ROP {time::Int,seq::Int} deriving Generic

paxos :: (Show st, (FromCli ::: st) ~ st, UnliftedS st ~ st)
      => Map String (st -> (st, String)) -- ^ Known operations
      -> PaxosConf st FromCli  -- ^ The configuration read from the command line interface
      -> Protocol "paxos"
paxos knownOps PaxosConf{..} = protocol @"paxos" do
  myself <- self

  puts "Waiting 60s for other nodes to come up..."
  delay 10000
  puts "Starting paxos..."

  setup oneshot timer ROP{time=6000, seq=1}

  upon timer \ROP{seq} -> do
    r <- random(0,1)
    op <- if r == 0 then pure "inc" else pure "dec"
    trigger commandRequest (myself, seq, StateOp op)
    seq' <- seq+1
    setup oneshot timer ROP{time=6000, seq=seq'}

  protocol @"replica" do -- A sub-protocol

    app_state <- new initialState
    slot_in   <- new 1
    slot_out  <- new 1
    requests  <- new (Set @Cmd)
    proposals <- new (Map @Int @Cmd)
    decisions <- new (Map @Int @Cmd)
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
            slot_in_copy <- get(slot_in)
            random_request <- randomElem(requests)
            requests -= random_request
            proposals += (slot_in_copy, random_request)
            foreach leaders \leader_host -> do
              trigger send ProposeMsg{to=leader_host, slot=slot_in_copy, cmd=random_request}
          slot_in := slot_in + 1

      perform((k,cid,op)) = do
        older_decision <- decisions `randomElemWith'` (\(slot,(k',cid',op')) ->
            slot < slot_out && k == k' && cid == cid' && op == op')
        if (isReconfig(op) || older_decision.exists) then do
          slot_in := slot_in + 1
        else do
          operation  <- lookup op.name knownOps
          state_copy <- get(app_state)
          let (next, result) = operation.value(state_copy)
          app_state := next
          puts ("New state: " ++ show next)
          slot_out := slot_out + 1
          trigger responseIndication result

    upon commandRequest \cmdName -> do
      requests += cmdName
      propose()

    upon receive \DecideMsg{slot, cmd} -> do
      puts ("Decided: " ++ show cmd)
      decisions += (slot, cmd)
      while (decisions `contains` slot_out) do
        c' <- decisions ! slot_out
        c'' <- lookup slot_out proposals
        when (c''.exists) do
          proposals -= slot_out
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
      proposals := props <| pmx
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
  puts "Starting a new commander..."
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
  puts "Starting a new scout..."
  myself   <- self
  wait_for <- new acceptors
  pvalues  <- new (Set @PValue)
  foreach acceptors \a -> do
    trigger send P1AMsg{to=a, leader=myself, ballot=b}

  upon receive \P1BMsg{from=a, ballot=b', accepted=r} -> do
    if b' == b then do
      pvalues := pvalues `union` r
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
        deriving (Eq, Ord, Show, Generic, Binary)

isReconfig(StateOp{}) = False
isReconfig(ReconfigOp{}) = True

commandRequest = request @Cmd "Command"
responseIndication = request @String "ClientResponse"

data ProposeMsg = ProposeMsg { to :: Host, slot :: Int, cmd :: Cmd } deriving (Generic, Binary, Show)
data DecideMsg  = DecideMsg  { to :: Host, slot :: Int, cmd :: Cmd } deriving (Generic, Binary, Show)

-- | Let a pvalue be a triple consisting of a ballot number, a slot number,
-- and a command
type PValue = (BN, Int, Cmd)

-- | Ballot number
type BN = (Int, Host)

data P1AMsg = P1AMsg { to :: Host, leader :: Host, ballot :: BN } deriving (Generic, Binary, Show)
data P1BMsg = P1BMsg { to :: Host, from   :: Host, ballot :: BN, accepted :: Set PValue } deriving (Generic, Binary, Show)

data P2AMsg = P2AMsg { to :: Host, leader :: Host, pv :: PValue } deriving (Generic, Binary, Show)
data P2BMsg = P2BMsg { to :: Host, from   :: Host, ballot :: BN } deriving (Generic, Binary, Show)

data PreemptedMsg = PreemptedMsg { to :: Host, ballot :: BN } deriving (Generic, Binary, Show)
data AdoptedMsg = AdoptedMsg { to :: Host, ballot :: BN, pvalues :: Set PValue } deriving (Generic, Binary, Show)

