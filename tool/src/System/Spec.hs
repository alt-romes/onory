{-# LANGUAGE LambdaCase, PatternSynonyms, ViewPatterns, UnicodeSyntax, DataKinds, TypeFamilies, TypeAbstractions, BlockArguments, FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-} -- Eq a => SEq a
module System.Spec
  ( module System.Spec
  -- re-exports, for now at the end.
  , Host, System, Set, Map

  -- re-export Prelude, hiding the functions we override.
  -- Importers should use no implicit prelude
  , module Prelude
  )
  where

import GHC.Records
import Type.Reflection
import Data.Kind
import Control.Monad hiding (when)
import Control.Monad.Free

import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M

import Prelude hiding ( (==), compare, (<=), (<), (>=), (>), (&&), (||), not
                      , all, any, filter
                      , (+), (-), (*), (/), mod, div, abs
                      )
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

-- | Send a message. The type of message sent must have
send :: ∀ msg. Typeable msg => Event msg
send = Message (typeRep @msg)

timer :: ∀ timer. Typeable timer => Event timer
timer = Timer (typeRep @timer)

setup :: HasField "time" timer Int {- time field in milliseconds -}
      => TimerType -> Event timer -> timer -> System ()
setup = undefined

periodic, oneshot :: TimerType
periodic = PeriodicTimer
oneshot  = OneShotTimer

random :: Container a => a -> System (Elem a)
random = undefined

ok :: System ()
ok = pure ()

--------------------------------------------------------------------------------
-- * Known channel events?

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
{-# INLINE when #-}

ifThenElse :: LiftS a Bool => a -> System b -> System b -> System b
ifThenElse sb st sf = do
  b <- liftS sb
  if b then st
       else sf
{-# INLINE ifThenElse #-}

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
  (+=) :: a -> Elem a -> AR a -- ToDo: Make me @a@
  (-=) :: a -> Key a  -> AR a
  size :: LiftS b a => b -> System Int
  contains :: a -> Key a -> System Bool
  notin    :: Key a -> a -> System Bool
  notin k c = P.not <$> contains c k
  {-# INLINE notin #-}
  union :: (LiftS b a, LiftS c a) => b -> c -> System a

  filter :: (LiftS b Bool, LiftS c a) => (Elem a -> b) -> c -> System a

instance P.Ord a => Container (Set a) where
  type Elem (Set a) = a
  type Key  (Set a) = a
  type AR   (Set a) = Set a
  foreach = forM_ . S.toList
  (+=) = flip S.insert
  (-=) = flip S.delete
  size = fmap S.size . liftS
  contains = fmap pure . flip S.member
  union x y = S.union <$> liftS x <*> liftS y

  filter f x = do
    sl <- S.toList <$> liftS x
    S.fromList <$> filterM (liftS . f) sl

  {-# INLINE foreach #-}
  {-# INLINE (+=) #-}
  {-# INLINE (-=) #-}
  {-# INLINE size #-}
  {-# INLINE contains #-}
  {-# INLINE union #-}
  {-# INLINE filter #-}

instance P.Ord k => Container (Map k a) where
  type Elem (Map k a) = (k, a)
  type Key  (Map k a) = k
  type AR   (Map k a) = Map k a
  foreach = forM_ . M.toList
  (+=) = flip $ uncurry M.insert
  (-=) = flip M.delete
  size = fmap M.size . liftS
  contains = fmap pure . flip M.member
  union x y = M.union <$> liftS x <*> liftS y
  filter f x = do
    ml <- M.toList <$> liftS x
    M.fromList <$> filterM (liftS . f) ml

  {-# INLINE foreach #-}
  {-# INLINE (+=) #-}
  {-# INLINE (-=) #-}
  {-# INLINE size #-}
  {-# INLINE contains #-}
  {-# INLINE union #-}
  {-# INLINE filter #-}

instance (Container a, AR a ~ a) => Container (Mutable a) where
  type Elem (Mutable a) = Elem a
  type Key  (Mutable a) = Key a
  type AR   (Mutable a) = System ()
  foreach ref f = do
    c <- get ref
    foreach c f
  (+=) ref x = modify ref (+= x)
  (-=) ref x = modify ref (-= x)
  size = size <=< get <=< liftS
  contains ref k = get ref >>= (`contains` k)

  -- | Union for Mutable containers will write the first reference with the union.
  union x y = do
    refx <- liftS x :: System (Mutable a)
    refy <- liftS y :: System (Mutable a)
    xc   <- get refx
    yc   <- get refy
    un   <- xc `union` yc
    refx := un
    return refx

  filter f x = do
    ref <- liftS x
    xc  <- get ref
    fc  <- filter f xc
    ref := fc
    return ref -- return the same ref that was given

  {-# INLINE foreach #-}
  {-# INLINE (+=) #-}
  {-# INLINE (-=) #-}
  {-# INLINE size #-}
  {-# INLINE contains #-}
  {-# INLINE union #-}
  {-# INLINE filter #-}

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

  (!=) x y = P.not <$> (x == y)
  {-# INLINE (!=) #-}

instance {-# OVERLAPPABLE #-} Eq a => SEq a where
  (==) x y = (P.==) <$> liftS x <*> liftS y
  {-# INLINE (==) #-}

instance {-# OVERLAPPING #-} SEq a => SEq (Mutable a) where
  (==) x y = do
    x' <- get =<< liftS x
    y' <- get =<< liftS y
    x' == y'
  {-# INLINE (==) #-}

class SOrd c where
  compare
    :: (LiftS a c, LiftS b c) => a -> b -> System Ordering

  (>), (<), (>=), (<=)
    :: (LiftS a c, LiftS b c) => a -> b -> System Bool

  x <= y = do
    c <- compare x y
    return $ case c of { GT -> False; _ -> True }
  x >= y = y <= x
  x > y = P.not <$> (x <= y)
  x < y = P.not <$> (y <= x)
  {-# INLINE (<=) #-}
  {-# INLINE (>=) #-}
  {-# INLINE (>) #-}
  {-# INLINE (<) #-}

instance {-# OVERLAPPABLE #-} P.Ord a => SOrd a where
  compare x y = P.compare <$> liftS x <*> liftS y
  {-# INLINE compare #-}

instance {-# OVERLAPPING #-} SOrd a => SOrd (Mutable a) where
  compare x y = do
    x' <- get =<< liftS x
    y' <- get =<< liftS y
    compare x' y'
  {-# INLINE compare #-}

(&&), (||) :: (LiftS a Bool, LiftS b Bool) => a -> b -> System Bool
(&&) a b = (P.&&) <$> liftS a <*> liftS b
(||) a b = (P.||) <$> liftS a <*> liftS b
{-# INLINE (&&) #-}
{-# INLINE (||) #-}

not :: LiftS a Bool => a -> System Bool
not a = P.not <$> liftS a
{-# INLINE not #-}

true, false :: Bool
true = True; false = False

(+), (-), (*) :: (Num n, LiftS a n, LiftS b n) => a -> b -> System n
(+) x y = (P.+) <$> liftS x <*> liftS y
(-) x y = (P.-) <$> liftS x <*> liftS y
(*) x y = (P.*) <$> liftS x <*> liftS y
{-# INLINE (+) #-}
{-# INLINE (-) #-}
{-# INLINE (*) #-}

(/) :: (Fractional n, LiftS a n, LiftS b n) => a -> b -> System n
(/) x y = (P./) <$> liftS x <*> liftS y
{-# INLINE (/) #-}

mod :: (Integral n, LiftS a n, LiftS b n) => a -> b -> System n
mod x y = P.mod <$> liftS x <*> liftS y
{-# INLINE mod #-}

div :: (Integral n, LiftS a n, LiftS b n) => a -> b -> System n
div x y = P.div <$> liftS x <*> liftS y
{-# INLINE div #-}

abs :: (Num n, LiftS a n) => a -> System n
abs x = P.abs <$> liftS x
{-# INLINE abs #-}

infixr 3 &&
infixr 2 ||
infix 4 <
infix 4 <=
infix 4 >
infix 4 >=
infix 4 ==
infix 4 !=
infixl 6 +
infixl 6 -
infixl 7 /

-- ... it feels like it kind of defeats the purpose, to have so many combinators overloaded, since we're redefining way too many things. oh well...

--------------------------------------------------------------------------------
-- Combinators

all :: (Traversable f, LiftS b Bool, LiftS c (f a)) => (a -> b) -> c ->  System Bool
all f t = P.and <$> (traverse (liftS . f) =<< liftS t)
{-# INLINE all #-}

any :: (Traversable f, LiftS b Bool, LiftS c (f a)) => (a -> b) -> c -> System Bool
any f t = P.or <$> (traverse (liftS . f) =<< liftS t)
{-# INLINE any #-}

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
  {-# INLINE liftS #-}

instance {-# OVERLAPPING #-} (a ~ b) => LiftS (System a) b where
  liftS = id
  {-# INLINE liftS #-}

