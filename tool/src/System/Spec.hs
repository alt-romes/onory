{-# LANGUAGE PatternSynonyms, ViewPatterns, UnicodeSyntax, DataKinds, TypeFamilies, TypeAbstractions, BlockArguments, RequiredTypeArguments #-}
module System.Spec where

import Type.Reflection
import Data.Kind
import Control.Monad
import Control.Monad.Free

import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M

import System.Spec.Free

--------------------------------------------------------------------------------
-- * Main interface

request :: ∀ req. Typeable req => Name -> System (Event req)
request name = declareEvent @req (Request name)

indication :: ∀ ind. Typeable ind => Name -> System (Event ind)
indication name = declareEvent @ind (Indication name)

self :: System Host
self = getSelf

new :: a -> System (Mutable a)
new = mkNew

upon :: Event n -> (n -> System ()) -> System ()
upon = uponEvent

receive :: forall msg_t -> Event msg_t
receive msg_t = Message (typeRep @msg_t)

ok :: System ()
ok = pure ()

--------------------------------------------------------------------------------
-- * Known events

neighbourUp :: Event (Set Host)
neighbourUp = Indication "neighbourUp"

neighbourDown :: Event (Set Host)
neighbourDown = Indication "neighbourDown"

--------------------------------------------------------------------------------
-- * State manipulation

modify :: Mutable a -> (a -> a) -> System ()
modify = modifyState

pattern (:=) :: Mutable a -> a -> System ()
pattern (:=) <- _ {- what does this direction mean -}
  where (:=) ref x = modifyState ref (const x)

(+=) :: Container a => Mutable a -> Elem a -> System ()
ref += x = modifyState ref (+: x)

(-=) :: Container a => Mutable a -> Key a -> System ()
ref -= x = modifyState ref (-: x)

--------------------------------------------------------------------------------
-- * Sets and maps

class Container a where
  type Elem a :: Type
  type Key  a :: Type
  foreach :: a -> (Elem a -> System b) -> System ()
  (+:) :: a -> Elem a -> a
  (-:) :: a -> Key a -> a
  size :: a -> Int

instance Container (Set a) where
  type Elem (Set a) = a
  type Key  (Set a) = a
  foreach = forM_ . S.toList
  (+:) = flip S.insert
  (-:) = flip S.delete
  size = S.size

instance Container (Map k a) where
  type Elem (Map k a) = (k, a)
  type Key  (Map k a) = k
  foreach = forM_ . M.toList
  (+:) = flip $ uncurry M.insert
  (-:) = flip M.delete
  size = M.size

-- | A new, empty, set
pattern Set :: Set a
pattern Set <- _ where
  Set = S.empty

-- | A new, empty, map
pattern Map :: Map k a
pattern Map <- _ where
  Map = M.empty

--------------------------------------------------------------------------------

data UUID
data Bytes
data FloodMessage = FM Host Host UUID Bytes

broadcast :: System ()
broadcast = do
  {-= Interface =-}
  -- Maybe these could just be top-level plain decls.
  -- That would also make it easier for procedures to be plainly at the top level
  broadcastRequest    <-
    request    @(UUID, Host, Bytes) "broadcastRequest"

  deliverNotification <-
    indication @(UUID, Host, Bytes) "deliverNotification"

  {-= State =-}
  myself     <- self
  neighbours <- new Set
  received   <- new Set

  {-= System =-}
  upon broadcastRequest \(mid, s, m) -> do
    processFloodMessage(myself, mid, s, m)

  upon (receive FloodMessage) \(FM from mid s m) -> do
    processFloodMessage(from, mid, s, m)
    
  upon neighbourUp \upNeighbours -> do
    foreach upNeighbours \h -> do
      neighbours += h

  upon neighbourDown \downNeighbours -> do
    foreach downNeighbours \h -> do
      neighbours -= h
    
processFloodMessage(from, mid, s, m) = do pure ()
--   if mid ∉ received then do
--     received <- received u {mid}
--     trigger deliverNotification(mid, s, m)

--     foreach neighbours \host -> do
--       if host != from then
--         trigger send@FloodMessage(host, mid, s, m)
--       else ok

--   else ok


-- ppr :: Show a => System a -> IO ()
-- ppr sys = do
--   _ <- iterM go sys
--   return ()
--   where
--     go :: SystemF (IO a) -> IO a
--     go (CreateRequest @t n f) = do
--       putStrLn ("new request " ++ show n ++ " of type " ++ show (typeRep @t))
--       f (UndReq n)
--     go (CreateIndication @t n f) = do
--       putStrLn ("new indication " ++ show n ++ " of type " ++ show (typeRep @t))
--       f (Indication n)
--     go (GetSelf f) = do
--       putStrLn "localhost"
--       f "localhost"
--     go (MkNew x f) = do
--       putStrLn ("new " ++ show x)
--       f x
--     go (UponReq x _f n) = do
--       putStrLn ("upon " ++ show x ++ " do ...")
--       n
--     go (UponInd x _f n) = do
--       putStrLn ("upon " ++ show x ++ " do ...")
--       n

