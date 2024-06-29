{-# LANGUAGE LambdaCase, PatternSynonyms, ViewPatterns, UnicodeSyntax, DataKinds, TypeFamilies, TypeAbstractions, BlockArguments, FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-} -- Eq a => SEq a et friends
-- todo: explicit export list of whole language features
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
import Data.Proxy
import Type.Reflection
import Data.Kind
import Control.Monad hiding (when)

import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M
import System.Random (Random)

import Prelude hiding ( (==), compare, (<=), (<), (>=), (>), (&&), (||), not
                      , all, any, filter
                      , (+), (-), (*), (/), mod, div, abs
                      , print
                      )
import qualified Prelude as P

import System.Spec.Free

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

upon :: Event n -> (n -> System a) -> System ()
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
      => TimerType timer -> Event timer -> timer -> System ()
setup = setupTimer

periodic :: HasField "repeat" timer Int {- repeat every n milliseconds -} => TimerType timer
periodic = PeriodicTimer

oneshot :: TimerType timer
oneshot  = OneShotTimer

cancel :: Event timer -> System ()
cancel = cancelTimer

random :: Random a => (a, a) -> System a
random = getRandom

logStr :: Int -> String -> System ()
logStr = traceStr

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
  type ImmutableCR a :: Type
  foreach :: a -> (Elem a -> System b) -> System ()
  (+=) :: a -> Elem a -> System a
  (-=) :: a -> Key a  -> System a
  empty :: System a
  size :: LiftS b a => b -> System Int
  contains :: a -> Key a -> System Bool

  notin    :: Key a -> a -> System Bool
  notin k c = P.not <$> contains c k
  {-# INLINE notin #-}

  union, (\\) :: (LiftS b a, LiftS c a) => b -> c -> System a

  toList :: (LiftS b a) => b -> System [Elem a]
  filter :: (LiftS b Bool, LiftS c a) => (Elem a -> b) -> c -> System a
  elToKey :: Proxy a -> Elem a -> Key a

instance P.Ord a => Container (Set a) where
  type Elem (Set a) = a
  type Key  (Set a) = a
  type ImmutableCR (Set a) = Set a
  foreach = forM_ . S.toList
  (+=) = fmap pure . flip S.insert
  (-=) = fmap pure . flip S.delete
  size = fmap S.size . liftS
  empty = pure S.empty
  contains = fmap pure . flip S.member
  union x y = S.union <$> liftS x <*> liftS y
  (\\) x y = (S.\\) <$> liftS x <*> liftS y
  toList x = S.toList <$> liftS x

  filter f x = do
    sl <- S.toList <$> liftS x
    S.fromList <$> filterM (liftS . f) sl
  elToKey _ = id

  {-# INLINE foreach #-}
  {-# INLINE (+=) #-}
  {-# INLINE (-=) #-}
  {-# INLINE empty #-}
  {-# INLINE size #-}
  {-# INLINE contains #-}
  {-# INLINE union #-}
  {-# INLINE (\\) #-}
  {-# INLINE toList #-}
  {-# INLINE filter #-}

instance P.Ord k => Container (Map k a) where
  type Elem (Map k a) = (k, a)
  type Key  (Map k a) = k
  type ImmutableCR (Map k a) = Map k a
  foreach = forM_ . M.toList
  (+=) c x = pure $ uncurry M.insert x c
  (-=) c x = pure $ M.delete x c
  size = fmap M.size . liftS
  empty = pure M.empty
  contains = fmap pure . flip M.member
  union x y = M.union <$> liftS x <*> liftS y
  (\\) x y = (M.\\) <$> liftS x <*> liftS y
  toList x = M.toList <$> liftS x
  filter f x = do
    ml <- M.toList <$> liftS x
    M.fromList <$> filterM (liftS . f) ml
  elToKey _ = fst

  {-# INLINE foreach #-}
  {-# INLINE (+=) #-}
  {-# INLINE (-=) #-}
  {-# INLINE empty #-}
  {-# INLINE size #-}
  {-# INLINE contains #-}
  {-# INLINE union #-}
  {-# INLINE (\\) #-}
  {-# INLINE toList #-}
  {-# INLINE filter #-}

instance Container a => Container (Mutable a) where
  type Elem (Mutable a) = Elem a
  type Key  (Mutable a) = Key a
  type ImmutableCR (Mutable a) = a
  foreach ref f = do
    c <- get ref
    foreach c f
  (+=) ref x = do
    c <- get ref
    c' <- c += x
    ref := c'
    return ref
  (-=) ref x = do
    c <- get ref
    c' <- c -= x
    ref := c'
    return ref
  empty = new =<< empty @a
  size = size <=< get <=< liftS
  contains ref k = get ref >>= (`contains` k)

  -- | Union for Mutable containers will write the first reference with the union.
  union x y = do
    refx <- liftS x
    refy <- liftS y
    xc   <- get refx
    yc   <- get refy
    un   <- xc `union` yc
    refx := un
    return refx
  -- | Difference for Mutable containers will write the first reference with the difference.
  (\\) x y = do
    refx <- liftS x
    refy <- liftS y
    xc   <- get refx
    yc   <- get refy
    un   <- xc \\ yc
    refx := un
    return refx
  toList = liftS >=> get >=> toList
  filter f x = do
    ref <- liftS x
    xc  <- get ref
    fc  <- filter f xc
    ref := fc
    return ref -- return the same ref that was given
  elToKey _ = elToKey (Proxy @a)

  {-# INLINE foreach #-}
  {-# INLINE (+=) #-}
  {-# INLINE (-=) #-}
  {-# INLINE empty #-}
  {-# INLINE size #-}
  {-# INLINE contains #-}
  {-# INLINE union #-}
  {-# INLINE (\\) #-}
  {-# INLINE toList #-}
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
infixl 9 \\

--------------------------------------------------------------------------------
-- Combinators

all :: (Traversable f, LiftS b Bool, LiftS c (f a)) => (a -> b) -> c ->  System Bool
all f t = P.and <$> (traverse (liftS . f) =<< liftS t)
{-# INLINE all #-}

any :: (Traversable f, LiftS b Bool, LiftS c (f a)) => (a -> b) -> c -> System Bool
any f t = P.or <$> (traverse (liftS . f) =<< liftS t)
{-# INLINE any #-}

--------------------------------------------------------------------------------
-- * Random

randomElem :: (Container a, LiftS b a) => b -> System (Elem a)
randomElem x = do
  c <- liftS x
  l <- toList (pure @System c)
  s <- size (pure @System c)
  i <- random (0, s P.- 1)
  return (l !! i)

randomElemWith :: (Container a, LiftS b a) => b -> (Elem a -> System Bool) -> System (Elem a)
randomElemWith container cond = do
  n <- randomElem container
  ifThenElse (cond n)
    (return n)
    (randomElemWith container cond)

-- | Extract a random subset out of this container.
-- If this is a mutable container, the reference will be written with the random subset.
randomSubset :: ∀ a b c. (Container a, LiftS b a, LiftS c Int, Container (ImmutableCR a), Elem a ~ Elem (ImmutableCR a))
             => (b, c) -> System (ImmutableCR a)
randomSubset (c, ln) = do
  container <- liftS c
  s :: Int <- size (pure @System container)
  n <- liftS ln
  let
    go :: ImmutableCR a -> Int -> System (ImmutableCR a)
    go !acc 0 = pure acc
    go !acc i = do
      r <- pure @System container `randomElemWith` \x -> elToKey (Proxy @(ImmutableCR a)) x `notin` acc
      acc' <- acc += r
      go acc' (i P.- 1)
  e <- empty @(ImmutableCR a)
  go e (min s n)

--------------------------------------------------------------------------------
-- * Logging

print :: Show a => a -> System ()
print = logStr 0 . show

puts :: String -> System ()
puts = logStr 0

trace :: String -> System ()
trace = logStr 1

--------------------------------------------------------------------------------
-- * Escape hatch

doIO :: IO a -> System a
doIO = escapeTheSystem

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

