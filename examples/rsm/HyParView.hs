{-# HLINT ignore "Redundant bracket" #-}
module HyParView where

import System.Distributed.Prelude

--------------------------------------------------------------------------------
-- Interface

neighbourUp   = indication @Host "neighbourUp"
neighbourDown = indication @Host "neighbourDown"

--------------------------------------------------------------------------------
-- Configuration

data HyParViewConf arg =
  HPVC
    { maxSizeActiveView :: arg ::: Int
        <?> "The max size of the active view"
    , maxSizePassiveView :: arg ::: Int
        <?> "The max size of the passive view"
    , actRWL :: arg ::: Int
        <?> "Active Random Walk Length"
    , passRWL :: arg ::: Int
        <?> "Passive Random Walk Length"
    , shuffleTimer :: arg ::: Int
        <!> "10000"
        <?> "Time in millis to trigger the event for passive view management (SHUFFLE)"
    , shuffleKa :: arg ::: Int
        <?> "The number of nodes from the active view sent in a Shuffle message."
    , shuffleKp :: arg ::: Int
        <?> "The number of nodes from the passive view sent in a Shuffle message."
    , shuffleTtl :: arg ::: Int
        <?> "The ttl for shuffle messages."
    , contactNode :: arg ::: Host
        <#> "c"
        <?> "The contact node"
    }
    deriving Generic

--------------------------------------------------------------------------------
-- Protocol

-- Paper: https://asc.di.fct.unl.pt/~jleitao/pdf/dsn07-leitao.pdf
-- Pseudo code: https://github.com/alt-romes/projeto-asd/blob/master/pseudo-code/HyParView.c#L89
hyParView :: HyParViewConf FromCli -> Protocol "HyParView"
hyParView HPVC{..} = protocol @"HyParView" do
  puts "Starting HyParView..."
  myself <- self
  puts ("I am " ++ show myself)

--------------------------------------------------------------------------------
-- State

  pending     <- new Set -- Set of pending nodes
  activeView  <- new Set -- Set of nodes in the active view
  passiveView <- new Set -- Set of nodes in the passive view

--------------------------------------------------------------------------------
-- Init

  pending += contactNode
  trigger send JoinMessage{ from=myself, to=contactNode, ttl=actRWL}
  setup periodic timer ShuffleTimer{time=shuffleTimer, repeat=shuffleTimer}
  
--------------------------------------------------------------------------------
-- View manipulation

  let
    addNodeActiveView(node) = do
      when (node != self && node `notin` activeView) do

        when isActiveViewFull do
          -- Drop random element from active view
          rn <- randomElem(activeView)
          trigger send DisconnectMessage{ from=myself, to=rn }
          call removeNodeActiveView(rn)
          passiveView += rn

        pending -= node
        activeView += node
        trigger neighbourUp(node)

    removeNodeActiveView(node) = do
        if activeView `contains` node then do
          activeView -= node
          trigger neighbourDown node
          return true
        else
          return false

    addNodePassiveView(node) = do
      when (node != myself && node `notin` activeView
            && not (passiveView `contains` node)) do
        when isPassiveViewFull do
          n <- randomElem(passiveView)
          passiveView -= n

        passiveView += node

    isActiveViewFull = activeViewSize >= maxSizeActiveView
    isPassiveViewFull = size passiveView >= maxSizePassiveView

    -- The active view size also counts with pending nodes
    activeViewSize = size activeView + size pending

--------------------------------------------------------------------------------
-- Handlers

  upon receive \JoinMessage{from=newNode, ttl} -> do
    call addNodeActiveView(newNode)
    trigger send JoinReplyMessage{ to=newNode, from=myself }

    foreach activeView \n -> do
      when (n != newNode) do
        trigger send ForwardJoinMessage{ from=myself, to=n,
                                         joined=newNode, ttl=actRWL }

  upon receive \JoinReplyMessage{from} -> do
    call addNodeActiveView(from)

  upon receive \ForwardJoinMessage{from=sender, joined=newNode, ttl} -> do
    if ttl == (0 :: Int) || activeViewSize == (1 :: Int) then -- this is kind of annoying...
      when (newNode `notin` pending && newNode `notin` activeView) do
        pending += newNode
        trigger send NeighbourMessage{from=myself, to=newNode, priority=true}
    else
      when (ttl == passRWL) do
        call addNodePassiveView(newNode)

        randomNode <-
          activeView `randomElemWith`
            \n -> n != sender && n != newNode
        ttl <- ttl - (1 :: Int)
        trigger send ForwardJoinMessage{to=randomNode, from=myself,
                                        joined=newNode, ttl}

  upon receive \NeighbourMessage{from=sender, priority} -> do
    addToActive <- priority && isActiveViewFull

    when addToActive do
      call addNodeActiveView(sender)

    trigger send NeighbourReplyMessage{to=sender, from=myself, accepted=addToActive}

  upon receive \NeighbourReplyMessage{from, accepted} -> do

    if pending `contains` from && accepted then do
      call addNodeActiveView(from)
      passiveView -= from
    else
      pending -= from

  upon receive \DisconnectMessage{from=sender} -> do
    when ( removeNodeActiveView(sender) ) do
      call addNodePassiveView(sender)

  -- TODO: TCPChannel Events bits
  -- Em qualquer evento de up ou down tcp channel chama-se a func promoteRandomNodeFromPassiveToActiveView().
  -- Em qualquer evento de down o node n é removido da active e passive view no caso de pertencer à active view.

  let
    -- Active View Management
    promoteRandomNodeFromPassiveToActiveView() = do
      when ( size passiveView > (0 :: Int) && not isActiveViewFull ) do
        node <- passiveView `randomElemWith` \n -> n `notin` pending
        pending += node

        priority <- activeViewSize <= (1 :: Int)
        trigger send NeighbourMessage{from=myself, to=node, priority}

    -- Passive View Management
    integrateReceivedNodesToPassiveView(receivedNodes, sentNodes) = do

      -- Something here is wrong because filtered is unused...
      filtered <- filter (\n ->
                      n != self &&
                        n `notin` activeView &&
                        n `notin` passiveView)
                    receivedNodes

      nodesToRemove <- size passiveView - maxSizePassiveView + size receivedNodes
      -- ToDo:!!! the rest of this method
      return ()

  upon timer \ShuffleTimer{time} -> do

    when (size activeView > (0 :: Int)) do
      activeViewNodes <- randomSubset(activeView, shuffleKa)
      passiveViewNodes <- randomSubset(passiveView, shuffleKp)

      host <- randomElem(activeView)

      nodesToSend <- activeViewNodes `union` passiveViewNodes

      trigger send ShuffleMessage{to=host, from=myself, ttl=shuffleTtl, nodes=nodesToSend}

  -- to:do: why did we have original sender vs sender??
  upon receive \ShuffleMessage{from=sender, nodes, ttl} -> do
    ttl <- ttl - (1 :: Int)
    if (ttl > (0::Int) && activeViewSize > (1::Int)) then do
       host <- activeView `randomElemWith` \h -> h != sender
       trigger send ShuffleMessage{to=host, from=sender, nodes, ttl}
    else do
      replyNodes <- randomSubset(passiveView, size nodes)
      -- send the received nodes in the reply
      trigger send ShuffleReplyMessage{to=sender, receivedNodes=nodes, replyNodes}
      call integrateReceivedNodesToPassiveView(nodes, replyNodes)

--------------------------------------------------------------------------------
-- Messages

data JoinMessage        = JoinMessage { from :: Host, to :: Host, ttl :: Int } deriving (Generic, Binary)
data ForwardJoinMessage = ForwardJoinMessage { from :: Host, to :: Host, joined :: Host, ttl :: Int } deriving (Generic, Binary)
data JoinReplyMessage   = JoinReplyMessage { from :: Host, to :: Host } deriving (Generic, Binary)
data DisconnectMessage  = DisconnectMessage { from :: Host, to :: Host } deriving (Generic, Binary)
data NeighbourMessage   = NeighbourMessage { from :: Host, to :: Host, priority :: Bool } deriving (Generic, Binary)
data NeighbourReplyMessage = NeighbourReplyMessage { from :: Host, to :: Host, accepted :: Bool } deriving (Generic, Binary)
data ShuffleMessage = ShuffleMessage{ from :: Host, to :: Host, ttl :: Int, nodes :: Set Host} deriving (Generic, Binary)
data ShuffleReplyMessage = ShuffleReplyMessage{ to :: Host, receivedNodes :: Set Host, replyNodes :: Set Host} deriving (Generic, Binary)

data ShuffleTimer = ShuffleTimer{ time :: Int, repeat :: Int}

