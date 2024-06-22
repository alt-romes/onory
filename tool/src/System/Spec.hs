{-# LANGUAGE UnicodeSyntax, DataKinds, TypeFamilies, TypeAbstractions, BlockArguments #-}
module System.Spec where

import Type.Reflection
import Data.Kind
import Control.Monad.Free
import System.Spec.Free
import System.Spec.Uponable

--------------------------------------------------------------------------------
-- Interface, proper

request :: ∀ req. Typeable req => Name -> System (Request UND req)
request = createRequest @req

indication :: ∀ t. Typeable t => Name -> System (Indication t)
indication = createIndication @t

self :: System Host
self = getSelf

new :: a -> System (Mutable a)
new = mkNew

upon :: Uponable n => n -> (UponableArgs n -> System ()) -> System ()
upon = uponImpl

ok :: System ()
ok = pure ()

neighbourUp :: Indication (Set Host)
neighbourUp = Indication "neighbourUp"

--------------------------------------------------------------------------------

broadcast :: System ()
broadcast = do
  {-= Interface =-}
  broadcastRequest    <- request    @(UUID, Host, Bytes) "broadcastRequest"
  deliverNotification <- indication @(UUID, Host, Bytes) "deliverNotification"

  {-= State =-}
  myself     <- self
  neighbours <- new [] -- OverloadedLists
  received   <- new []

  {-= System =-}
  upon broadcastRequest \(mid, s, m) -> do
    processFloodMessage(myself, mid, s, m)

  -- upon receive@FloodMessage \(from, mid, s, m) -> do
  --   processFloodMessage(from, mid, s, m)
    
  upon neighbourUp \upNeighbours -> do
    foreach upNeighbours \h -> do
      neighbours <- neighbours u [h]

  upon neighbourDown \downNeighbours -> do
    foreach downNeighbours \h -> do
      neighbours <- neighbours \\ [h]
    
processFloodMessage(from, mid, s, m) = do
  if mid ∉ received then do
    received <- received u {mid}
    trigger deliverNotification(mid, s, m)

    foreach neighbours \host -> do
      if host != from then
        trigger send@FloodMessage(host, mid, s, m)
      else ok

  else ok


ppr :: Show a => System a -> IO ()
ppr sys = do
  _ <- iterM go sys
  return ()
  where
    go :: SystemF (IO a) -> IO a
    go (CreateRequest @t n f) = do
      putStrLn ("new request " ++ show n ++ " of type " ++ show (typeRep @t))
      f (UndReq n)
    go (CreateIndication @t n f) = do
      putStrLn ("new indication " ++ show n ++ " of type " ++ show (typeRep @t))
      f (Indication n)
    go (GetSelf f) = do
      putStrLn "localhost"
      f "localhost"
    go (MkNew x f) = do
      putStrLn ("new " ++ show x)
      f x
    go (UponReq x _f n) = do
      putStrLn ("upon " ++ show x ++ " do ...")
      n
    go (UponInd x _f n) = do
      putStrLn ("upon " ++ show x ++ " do ...")
      n

