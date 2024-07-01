{-# LANGUAGE BlockArguments, TypeApplications, RebindableSyntax, DeriveGeneric, DeriveAnyClass #-}
module Main (main) where

import System.Distributed.Prelude

main :: IO ()
main = putStrLn "All typechecks!"

--------------------------------------------------------------------------------

newtype UUID = UUID Int deriving (Eq, Ord, Generic, Binary)
data Bytes deriving (Generic, Binary)
data FloodMessage = FM { to :: Host, i :: UUID, o :: Host, bs :: Bytes } deriving (Generic, Binary)


{-= Interface =-}
broadcastRequest    = request    @(UUID, Host, Bytes) "broadcastRequest"
deliverNotification = indication @(UUID, Host, Bytes) "deliverNotification"
neighbourUp         = indication @(Set Host)          "neighbourUp"
neighbourDown       = indication @(Set Host)          "neighbourDown"

broadcast :: System ()
broadcast = do

  {-= State =-}
  myself     <- self
  neighbours <- new Set
  received   <- new Set

  {-= Procedures =-}
  let
    processFloodMessage(FM from mid s m) = do

      when (mid `notin` received) do

        received += mid
        trigger deliverNotification(mid, s, m)

        foreach neighbours \host -> do

          when (host != from) do
            trigger (send @FloodMessage) (FM host mid s m)

  {-= System =-}
  upon broadcastRequest \(mid, s, m) -> do
    call processFloodMessage(FM myself mid s m)

  upon (receive @FloodMessage) \(FM from mid s m) -> do
    call processFloodMessage(FM from mid s m)
    
  upon neighbourUp \upNeighbours -> do
    foreach upNeighbours \h -> do
      neighbours += h

  upon neighbourDown \downNeighbours -> do
    foreach downNeighbours \h -> do
      neighbours -= h

