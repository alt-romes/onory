-- ROMES:TODO: Pass these options in the GHC wrapper automatically.
{-# LANGUAGE LambdaCase, RecordWildCards, OverloadedRecordDot, BlockArguments, NoImplicitPrelude, RebindableSyntax, DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-missing-signatures -Wno-unused-do-bind -Wno-name-shadowing -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
module Main where

import System.Environment (getArgs)
import System.Distributed.Interpret
import System.Distributed.Prelude

default (Int)

main :: IO ()
main = do
  getArgs >>= \case
    [port] ->
      runLebab SysConf{verbosity=4, hostname="localhost", port=read port}
        [ hyParView (HPVC 10 100 10 10 500 5 5 5) (host "localhost" 25002)
        ]
    _ -> error "Usage: ./Main <port>"

type Node = Host

--------------------------------------------------------------------------------
-- Interface

neighbourUp   = indication @Node "neighbourUp"
neighbourDown = indication @Node "neighbourDown"

--------------------------------------------------------------------------------
-- Configuration

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

--------------------------------------------------------------------------------
-- Protocol

-- Paper: https://asc.di.fct.unl.pt/~jleitao/pdf/dsn07-leitao.pdf
-- Pseudo code: https://github.com/alt-romes/projeto-asd/blob/master/pseudo-code/HyParView.c#L89
hyParView HPVC{..} contactNode = protocol "HyParView" do
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
      activeViewNodes :: Set Node <- randomSubset(activeView, shuffleKa)
      passiveViewNodes :: Set Node <- randomSubset(passiveView, shuffleKp)

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

data JoinMessage        = JoinMessage { from :: Host, to :: Host, ttl :: Int }
data ForwardJoinMessage = ForwardJoinMessage { from :: Host, to :: Host, joined :: Host, ttl :: Int }
data JoinReplyMessage   = JoinReplyMessage { from :: Host, to :: Host }
data DisconnectMessage  = DisconnectMessage { from :: Host, to :: Host }
data NeighbourMessage   = NeighbourMessage { from :: Host, to :: Host, priority :: Bool }
data NeighbourReplyMessage = NeighbourReplyMessage { from :: Host, to :: Host, accepted :: Bool }
data ShuffleMessage = ShuffleMessage{ from :: Host, to :: Host, ttl :: Int, nodes :: Set Node}
data ShuffleReplyMessage = ShuffleReplyMessage{ to :: Host, receivedNodes :: Set Node, replyNodes :: Set Node}
data ShuffleTimer = ShuffleTimer{ time :: Int, repeat :: Int}

--------------------------------------------------------------------------------
-- Helpers

