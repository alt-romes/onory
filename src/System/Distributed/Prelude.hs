{-# LANGUAGE LambdaCase, PatternSynonyms, ViewPatterns, UnicodeSyntax, DataKinds, TypeFamilies, BlockArguments, FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-} -- Eq a => SEq a et friends
{-# OPTIONS_GHC -Wno-orphans #-} -- Eq a => SEq a et friends
-- todo: explicit export list of whole language features
module System.Distributed.Prelude
  ( module System.Distributed.Core
  , module System.Distributed.Prelude
  -- re-exports, for now at the end.
  , Host, System, Protocol, Set, Map

  , Generic {-ParseRecord: we need only Generic for confs-}
  , Binary
  -- re-export optparse-generic niceties
  , type (<?>), type (<!>), type (<#>), type (:::)

  -- * Prelude re-export
  -- | We also re-export the rest of the prelude (with the exception of the
  -- functions we have re-defined).
  , module Prelude
  , module GHC.Records
  )
  where

import GHC.Records
import GHC.Generics (Generic)
import Data.Binary (Binary)
import Data.Proxy
import Data.Kind
import Type.Reflection
import qualified Data.Text as T
import Text.Read (readMaybe)
import Control.Monad hiding (when)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M
import Options.Generic (type (<?>), type (<!>), type (<#>), type (:::), Unwrapped, ParseRecord(..), getOnly, ParseFields(..), ParseField(..))

import Prelude hiding ( (==), compare, (<=), (<), (>=), (>), (&&), (||), not
                      , all, any, filter
                      , (+), (-), (*), (/), mod, div, abs
                      , print, lookup
                      )
import qualified Prelude as P

import System.Distributed.Free
import System.Distributed.Core
import Data.Maybe

type FromCli = Unwrapped

--------------------------------------------------------------------------------
-- * Control flow

while :: LiftS a Bool => a -> System b -> System ()
while sb sc = do
  b <- liftS sb
  if b then sc >> while sb sc
       else pure ()
{-# INLINE while #-}

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
-- * Mutability

-- | Let's sell it as "copying".
-- Whenever we want to use the value of a mutable reference immutabily, for
-- instance, to send in a message, or to store in a different variable, we need
-- to copy the value.
copy :: Mutable a -> System a
copy = get

--------------------------------------------------------------------------------
-- * Sets and maps

class Container a where
  type Elem a :: Type
  type Key  a :: Type
  type ImmutableCR a :: Type
  -- | For each element in a collection
  --
  -- @
  -- myset <- 'new' Set
  -- myset '+=' (1::Int)
  -- myset '+=' (2::Int)
  -- 'foreach' myset \elem -> do
  --   'print' elem
  -- @
  foreach :: a -> (Elem a -> System b) -> System ()
  (+=) :: a -> Elem a -> System a
  (-=) :: a -> Key a  -> System a
  empty :: System a
  size :: LiftS b a => b -> System Int
  contains :: (LiftS b (Key a), LiftS c a) => c -> b -> System Bool

  notin    :: (LiftS b (Key a), LiftS c a) => b -> c -> System Bool
  notin k c = do
    P.not <$> contains c k
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
  contains sc sk = do
    c <- liftS sc
    k <- liftS sk
    return $ S.member k c
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
  contains sc sk = do
    c <- liftS sc
    k <- liftS sk
    return $ M.member k c
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
  contains ref k = liftS ref >>= get >>= (`contains` k)

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

class Container a => MapContainer a where
  type Lookup a
  lookup :: (LiftS b (Key a), LiftS c a) => b -> c -> System (Maybe (Lookup a))

instance Ord k => MapContainer (Map k a) where
  type Lookup (Map k a) = a
  lookup sk sm = do
    k <- liftS sk
    m <- liftS sm
    pure $ M.lookup k m

instance MapContainer a => MapContainer (Mutable a) where
  type Lookup (Mutable a) = Lookup a
  lookup sk sm = lookup sk (liftS sm >>= get)

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
      r <- pure @System container `randomElemWith` \x ->
        notin (pure @System $ elToKey (Proxy @(ImmutableCR a)) x) (pure @System acc)
      acc' <- acc += r
      go acc' (i P.- 1)
  e <- empty @(ImmutableCR a)
  go e (min s n)

--------------------------------------------------------------------------------
-- * Utilities

-- | Turn any value into a System one by automatically @return@ing it if
-- needed. The lifted value is @b@.
class LiftS a b | a -> b where
  -- | Not /that/ SystemV.
  -- The value that is lifted in(to) the system.
  liftS :: a -> System b

instance {-# OVERLAPPABLE #-} (a ~ b) => LiftS a b where
  liftS = pure
  {-# INLINE liftS #-}

instance {-# OVERLAPPING #-} (a ~ b) => LiftS (Mutable a) b where
  liftS = get
  {-# INLINE liftS #-}

instance {-# OVERLAPPING #-} (a ~ b) => LiftS (System a) b where
  liftS = id
  -- we could check typeable for a = Mutable and get the value here, if anyone
  -- ever wanted to add together a System (Mutable Int) and an Int...
  {-# INLINE liftS #-}

-- Doesn't work.
-- instance {-# OVERLAPPING #-} (a ~ Mutable b, b ~ c) => LiftS (System a) c where
--   liftS = (get =<<)
--   {-# INLINE liftS #-}

instance (Read a, Ord a, Typeable a) => ParseRecord (Set a) where
  parseRecord = fmap getOnly parseRecord

instance (Read a, Ord a, Typeable a) => ParseFields (Set a) -- default impl uses ParseField
instance (Read a, Ord a, Typeable a) => ParseField (Set a) where
  parseField hl fl sn dv =
    (\s -> S.fromList $
      fromMaybe (error $ "Failed to read from the command line option " ++ show fl ++ " a " ++ show (typeRep @a) ++ " in a comma separated set of values.")
      . readMaybe @a . T.unpack <$> T.split (P.== ',') s)
    <$> parseField @T.Text hl fl sn dv

