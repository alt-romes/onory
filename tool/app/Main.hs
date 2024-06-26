{-# LANGUAGE RecordWildCards, OverloadedRecordDot, BlockArguments, NoImplicitPrelude, RebindableSyntax, DuplicateRecordFields #-}
module Main where

-- import System.Spec.Interpret
import System.Spec

main :: IO ()
main = do
  putStrLn "Hello, System!"

type Node = Host

neighbourUp   = indication @Node "neighbourUp"
neighbourDown = indication @Node "neighbourDown"

data HyParViewConf =
  HPVC
    { maxSizeActiveView :: Int -- ^ The max size of the active view
    , maxSizePassiveView :: Int -- ^ The max size of the passive view
    , actRWL :: Int -- ^ Active Random Walk Length
    , passRWL :: Int -- ^ Passive Random Walk Length
    , shuffleTimer :: Int -- ^ Time in millis to trigger the event for passive view management (SHUFFLE)
    , shuffleKa :: Int -- ^ The number of nodes from the active view sent in a Shuffle message.
    , shuffleKp :: Int -- The number of nodes from the passive view sent in a Shuffle message.
    , shuffleTtl :: Int -- The ttl for shuffle messages.
    }

-- Based on https://github.com/alt-romes/projeto-asd/blob/master/pseudo-code/HyParView.c#L89
hyParView :: HyParViewConf
          -> Host -- ^ The contact node
          -> System ()
hyParView HPVC{..} contactNode = do
  myself <- self

  pending     <- new Set -- Set of pending nodes
  activeView  <- new Set -- Set of nodes in the active view
  passiveView <- new Set -- Set of nodes in the passive view

  {-= Init =-}
  pending += contactNode
  trigger send JoinMessage{ from=myself, to=contactNode, ttl=actRWL}
  -- setup periodic timer ShuffleTimer(shuffleTimer)
  
  {-= Procedures =-}
  let

    {-= View manipulation primitives =-}
    addNodeActiveView(node::Host) = do
      when (node != self && node `notin` activeView) do

        when ( isActiveViewFull() ) do
          -- Drop random element from active view
          rn <- random(activeView)
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
          return True
        else
          return False

    addNodePassiveView(node) = do
      when (node != myself && node `notin` activeView
            && not (passiveView `contains` node)) do
        when ( isPassiveViewFull() ) do
          n <- random(passiveView)
          passiveView -= n

        passiveView += node

    isActiveViewFull() = activeViewSize() >= maxSizeActiveView
    isPassiveViewFull() = size passiveView >= maxSizePassiveView

    -- The active view size also counts with pending nodes
    activeViewSize() = do
      s1 <- size activeView
      s2 <- size pending
      return (s1 + s2)
      

  {-= Handlers =-}
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
    if ttl == (0::Int) || activeViewSize() == (1::Int) then -- this is kind of annoying...
      when (newNode `notin` pending && newNode `notin` activeView) do
        pending += newNode
        trigger send NeighbourMessage{from=myself, to=newNode, priority=true}
    else
      when (ttl == passRWL) do
        call addNodePassiveView(newNode)

        randomNode <- randomWith(activeView, \n -> n != sender && n != newNode)
        trigger send ForwardJoinMessage{to=randomNode, from=myself,
                                        joined=newNode, ttl=ttl-1}

  upon receive \NeighbourMessage{from=sender, priority} -> do
    addToActive <- priority && isActiveViewFull()

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
      when ( size passiveView > (0 :: Int) && not(isActiveViewFull()) ) do
        node <- randomWith(passiveView, \n -> n `notin` pending)
        pending += node

        priority <- activeViewSize() <= (1 :: Int)
        trigger send NeighbourMessage{from=myself, to=node, priority}

    -- Passive View Management
    integrateReceivedNodesToPassiveView(receivedNodes, sentNodes) = do
      -- ToDo
      return ()

    -- ToDo: from Upon Timer ShuffleTimer

  return ()

--------------------------------------------------------------------------------
-- Messages

data JoinMessage        = JoinMessage { from :: Host, to :: Host, ttl :: Int }
data ForwardJoinMessage = ForwardJoinMessage { from :: Host, to :: Host, joined :: Host, ttl :: Int }
data JoinReplyMessage   = JoinReplyMessage { from :: Host, to :: Host }
data DisconnectMessage  = DisconnectMessage { from :: Host, to :: Host }
data NeighbourMessage   = NeighbourMessage { from :: Host, to :: Host, priority :: Bool }
data NeighbourReplyMessage = NeighbourReplyMessage { from :: Host, to :: Host, accepted :: Bool }

--------------------------------------------------------------------------------
-- Helpers

randomWith :: Container a => (a, Elem a -> System Bool) -> System (Elem a)
randomWith (container, cond) = do
    n <- random(container)
    if cond(n) then
      return n
    else
      randomWith(container, cond)
