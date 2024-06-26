{-# LANGUAGE PatternSynonyms, ViewPatterns, UnicodeSyntax, DataKinds, TypeFamilies, TypeAbstractions, BlockArguments, FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-} -- Eq a => SEq a
module System.Spec
  ( module System.Spec
  -- re-exports, for now at the end.
  , Host, System

  -- re-export Prelude, hiding the functions we override.
  -- Importers should use no implicit prelude
  , module Prelude
  )
  where

import Type.Reflection
import Data.Kind
import Control.Monad hiding (when)
import Control.Monad.Free

import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M

import Prelude hiding ((==), compare, (<=), (<), (>=), (>))
import qualified Prelude as P

import System.Spec.Free
import System.Spec.Interpret

--------------------------------------------------------------------------------
-- * Main interface

request :: ∀ req. Typeable req => Name -> Event req
request name = Request name (typeRep @req)

indication :: ∀ ind. Typeable ind => Name -> Event ind
indication name = Indication name (typeRep @ind)

self :: System Host
self = getSelf

new :: a -> System (Mutable a)
new = mkNew

upon :: Event n -> (n -> System ()) -> System ()
upon = uponEvent

trigger :: Event n -> n -> System ()
trigger = triggerEvent

receive :: ∀ msg. Typeable msg => Event msg
receive = Message (typeRep @msg)

send :: ∀ msg. Typeable msg => Event msg
send = Message (typeRep @msg)

ok :: System ()
ok = pure ()

--------------------------------------------------------------------------------
-- * Known events?

--- ...

--------------------------------------------------------------------------------
-- * State manipulation

modify :: Mutable a -> (a -> a) -> System ()
modify = modifyState 

get :: Mutable a -> System a
get = getState 

pattern (:=) :: Mutable a -> a -> System ()
pattern (:=) <- _ {- what does this direction mean -}
  where (:=) ref x = modify ref (const x)

--------------------------------------------------------------------------------
-- * Control flow

when :: LiftS a Bool => a -> System b -> System ()
when sb sc = do
  b <- liftS sb
  if b then void sc
       else pure ()

ifThenElse :: LiftS a Bool => a -> System b -> System c -> System ()
ifThenElse sb st sf = do
  b <- liftS sb
  if b then void st
       else void sf

-- | A placebo
call :: ∀ a. a -> a
call = id

--------------------------------------------------------------------------------
-- * Sets and maps

class Container a where
  type Elem a :: Type
  type Key  a :: Type
  type AR   a :: Type
  foreach :: a -> (Elem a -> System b) -> System ()
  (+=) :: a -> Elem a -> AR a
  (-=) :: a -> Key a  -> AR a
  size :: a -> System Int
  contains :: a -> Key a -> System Bool
  notin    :: Key a -> a -> System Bool
  notin k c = not <$> contains c k

instance P.Ord a => Container (Set a) where
  type Elem (Set a) = a
  type Key  (Set a) = a
  type AR   (Set a) = Set a
  foreach = forM_ . S.toList
  (+=) = flip S.insert
  (-=) = flip S.delete
  size = pure . S.size
  contains = fmap pure . flip S.member

instance P.Ord k => Container (Map k a) where
  type Elem (Map k a) = (k, a)
  type Key  (Map k a) = k
  type AR   (Map k a) = Map k a
  foreach = forM_ . M.toList
  (+=) = flip $ uncurry M.insert
  (-=) = flip M.delete
  size = pure . M.size
  contains = fmap pure . flip M.member

instance (Container a, AR a ~ a) => Container (Mutable a) where
  type Elem (Mutable a) = Elem a
  type Key  (Mutable a) = Key a
  type AR   (Mutable a) = System ()
  foreach ref f = do
    c <- get ref
    foreach c f
  (+=) ref x = modify ref (+= x)
  (-=) ref x = modify ref (-= x)
  size = get >=> size
  contains ref k = get ref >>= (`contains` k)

-- | A new, empty, set
pattern Set :: Set a
pattern Set <- _ where
  Set = S.empty

-- | A new, empty, map
pattern Map :: Map k a
pattern Map <- _ where
  Map = M.empty

--------------------------------------------------------------------------------
-- * Expressions

class SEq c where
  (==), (!=)
    :: (LiftS a c, LiftS b c) => a -> b -> System Bool

  (!=) x y = not <$> x == y

instance {-# OVERLAPPABLE #-} Eq a => SEq a where
  (==) x y = (P.==) <$> liftS x <*> liftS y

instance {-# OVERLAPPING #-} SEq a => SEq (Mutable a) where
  (==) x y = do
    x' <- get =<< liftS x
    y' <- get =<< liftS y
    x' == y'

class SOrd c where
  compare
    :: (LiftS a c, LiftS b c) => a -> b -> System Ordering

  (>), (<), (>=), (<=)
    :: (LiftS a c, LiftS b c) => a -> b -> System Bool

  x <= y = do
    c <- compare x y
    return $ case c of { GT -> False; _ -> True }
  x >= y = y <= x
  x > y = not <$> x <= y
  x < y = not <$> y <= x

instance {-# OVERLAPPABLE #-} P.Ord a => SOrd a where
  compare x y = P.compare <$> liftS x <*> liftS y

instance {-# OVERLAPPING #-} SOrd a => SOrd (Mutable a) where
  compare x y = do
    x' <- get =<< liftS x
    y' <- get =<< liftS y
    compare x' y'

--------------------------------------------------------------------------------
-- * Utilities

-- | Turn any value into a System one by automatically @return@ing it if
-- needed. The lifted value is `b`.
class LiftS a b | a -> b where
  -- | Not /that/ SystemV.
  -- The value that is lifted in(to) the system.
  liftS :: a -> System b

instance {-# OVERLAPPABLE #-} (a ~ b) => LiftS a b where
  liftS = pure

instance {-# OVERLAPPING #-} (a ~ b) => LiftS (System a) b where
  liftS = id

