module Main (main) where

import System.Spec

main :: IO ()
main = putStrLn "All typechecks!"

-- * Ensure these typecheck, eventually run them.

-- The invocations of the tool will need to pass -XNoImplicitPrelude,
-- -XRebindableSyntax?, -XOverloadedLists et friends. Possibly even
-- pre-processing definitions (like -Dsomething ...).
system1 :: System ()
system1 = do

  {-= Interface =-}
  broadcastRequest    <- request    @(UUID, Host, Bytes) "broadcastRequest"
  deliverNotification <- indication @(UUID, Host, Bytes) "deliverNotification"

  {-= State =-}
  myself     <- self
  neighbours <- new [] -- OverloadedLists
  received   <- new []
  received   <- new 0

  {-= System =-}
  upon broadcastRequest \(mid, s, m) -> do
    -- if channelReady then
      call processFloodMessage(myself, mid, s, m)

  upon receive@FloodMessage \(from, mid, s, m) -> do
      call processFloodMessage(from, mid, s, m)
    
  procedure processFloodMessage \(from, mid, s, m) -> do
    if mid ∉ received then
      received <- received u {mid}
      trigger deliverNotification(mid, s, m)

      foreach neighbours \host -> do
        if host != from then
          trigger send@FloodMessage(host, mid, s, m)
        else ok

    else ok

  upon neighbourUp \upNeighbours -> do
    foreach upNeighbours \h -> do
      neighbours <- neighbours u [h]

  upon neighbourDown \downNeighbours -> do
    foreach downNeighbours \h -> do
      neighbours <- neighbours \\ [h]
    

