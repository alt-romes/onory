{-# LANGUAGE LambdaCase, PatternSynonyms, ViewPatterns, UnicodeSyntax, DataKinds, TypeFamilies, BlockArguments, FunctionalDependencies, DeriveAnyClass #-}
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
import System.Distributed.Core hiding ((:=))
import qualified System.Distributed.Core as Core
import Data.Maybe

default (Int)

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
-- * Mutability, utils, etc...

-- Redefine with LiftS
pattern (:=) :: LiftS b a => Mutable a -> b -> System ()
pattern (:=) <- _ {- what does this direction mean -}
  where (:=) ref x = liftS x >>= (ref Core.:=)

infixr 0 :=

-- | Construct a pair
(***) :: (LiftS b a, LiftS d c) => b -> d -> System (a, c)
(***) a b = (,) <$> liftS a <*> liftS b

infixl 8 ***

-- | Match on a value
--
-- @
-- (a, b) <- match some_tuple
-- @
match :: (LiftS b a) => b -> System a
match = liftS

--------------------------------------------------------------------------------
-- * Sets and maps

class (Elem (ImmutableCR a) ~ Elem a) => Container a where
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
  (+=) :: LiftS b (Elem a) => a -> b -> System a
  (-=) :: LiftS b (Key a)  => a -> b -> System a
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

  -- internal
  elToKey :: Proxy a -> Elem a -> Key a
  immutableCR :: a -> System (ImmutableCR a)

instance P.Ord a => Container (Set a) where
  type Elem (Set a) = a
  type Key  (Set a) = a
  type ImmutableCR (Set a) = Set a
  foreach = forM_ . S.toList
  (+=) c = liftS >=> pure . flip S.insert c
  (-=) c = liftS >=> pure . flip S.delete c
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
  immutableCR = pure

instance P.Ord k => Container (Map k a) where
  type Elem (Map k a) = (k, a)
  type Key  (Map k a) = k
  type ImmutableCR (Map k a) = Map k a
  foreach = forM_ . M.toList
  (+=) c x = liftS x >>= \x' -> pure $ uncurry M.insert x' c
  (-=) c x = liftS x >>= \x' -> pure $ M.delete x' c
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
  immutableCR = pure

instance (UnliftedS a ~ a, Container a) => Container (Mutable a) where
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
  size = size @a <=< get <=< liftS
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
  immutableCR = get

-- | A new, empty, set
pattern Set :: Set a
pattern Set <- _ where
  Set = S.empty

setFromList :: Ord a => [a] -> Set a
setFromList = S.fromList

-- | A new, empty, map
pattern Map :: Map k a
pattern Map <- _ where
  Map = M.empty

mapFromList :: Ord k => [(k, a)] -> Map k a
mapFromList = M.fromList

class Container a => MapContainer a where
  type Lookup a
  lookup :: (LiftS b (Key a), LiftS c a) => b -> c -> System (NullableValue (Lookup a))

instance Ord k => MapContainer (Map k a) where
  type Lookup (Map k a) = a
  lookup sk sm = do
    k <- liftS sk
    m <- liftS sm
    case M.lookup k m of
      Nothing -> pure NullValue
      Just x  -> pure (ExistsValue x)

instance (UnliftedS a ~ a, MapContainer a) => MapContainer (Mutable a) where
  type Lookup (Mutable a) = Lookup a
  lookup sk sm = lookup sk (liftS sm >>= get)

(!) :: (MapContainer a, LiftS b (Key a), LiftS c a) => c -> b -> System (Lookup a)
(!) x y = do
  r <- lookup y x
  case r of
    NullValue -> error "Failed to find key in map in some expression map ! key" -- ++ show k ++ " in map " ++ show m
    ExistsValue v -> return v

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

instance {-# OVERLAPPING #-} (UnliftedS a ~ a, SEq a) => SEq (Mutable a) where
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

instance {-# OVERLAPPING #-} (UnliftedS a ~ a, SOrd a) => SOrd (Mutable a) where
  compare x y = do
    x' <- get =<< liftS x
    y' <- get =<< liftS y
    compare @a x' y'
  {-# INLINE compare #-}

instance {-# OVERLAPPING #-} SOrd Int where
  compare x y = P.compare <$> liftS x <*> liftS y
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

-- | Take a random element from a container.
-- If the container is empty the thread running this instruction will crash (you
-- may be looking for "randomElem'" instead).
randomElem :: (Container a, LiftS b a) => b -> System (Elem a)
randomElem x = do
  flip orDefault (error "randomElemWith: container is empty!") <$>
    randomElem' x

-- | Take a random element from a container under a given condition.
-- If no element is available the thread running this instruction will crash.
--
-- Using "randomElemWith'" is preferred.
randomElemWith :: (Container a, LiftS b a, Container (ImmutableCR a), UnliftedS (ImmutableCR a) ~ ImmutableCR a, LiftS (ImmutableCR a) (UnliftedS (ImmutableCR a)), LiftS (Key (ImmutableCR a)) (Key (ImmutableCR a)))
               => b -> (Elem a -> System Bool) -> System (Elem a)
randomElemWith container cond =
  flip orDefault (error "randomElemWith: container is empty!") <$>
    randomElemWith' container cond

-- | Take a random element from a container.
-- If the container is empty, returns null.
randomElem' :: ∀ a b. (Container a, LiftS b a) => b -> System (NullableValue (Elem a))
randomElem' x = do
  c <- liftS x
  l <- toList (pure @System c)
  s <- size (pure @System c)
  if s P.> 0 then do
    i <- random (0, s P.- 1)
    return $ ExistsValue (l !! i)
  else do
    return NullValue

-- | Take a random element from a container under a given condition.
-- If no element is available, returns null.
randomElemWith' :: ∀ a b. (Container a, LiftS b a, Container (ImmutableCR a), UnliftedS (ImmutableCR a) ~ ImmutableCR a, LiftS (ImmutableCR a) (UnliftedS (ImmutableCR a)), LiftS (Key (ImmutableCR a)) (Key (ImmutableCR a))) -- these instances are needed because they are type families probably
                => b -> (Elem a -> System Bool) -> System (NullableValue (Elem a))
randomElemWith' container cond = do
  c <- liftS container
  ic <- immutableCR c
  go ic
  where
    go :: ImmutableCR a -> System (NullableValue (Elem a))
    go ic = do
      mn <- randomElem' ic
      case mn of
        NullValue -> return NullValue
        ExistsValue n -> do
          ic' <- ic -= elToKey (Proxy @(ImmutableCR a)) n
          ifThenElse (cond n)
            (return (ExistsValue n))
            (go ic')

-- | Extract a random subset out of this container. Returns an immutable copy of
-- the container random subset.
-- If this is a mutable container, the reference will be unchanged.
randomSubset :: ∀ a b c. (Container a, LiftS b a, LiftS c Int, Container (ImmutableCR a),
                UnliftedS (ImmutableCR a) ~ ImmutableCR a, Elem a ~ Elem (ImmutableCR a), (Key (UnliftedS (ImmutableCR a)) ~ UnliftedS (Key (ImmutableCR a))), (LiftS (Key (ImmutableCR a)) (UnliftedS (Key (ImmutableCR a))), LiftS (ImmutableCR a) (UnliftedS (ImmutableCR a))), LiftS (Elem a) (Elem a))
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
        notin (elToKey (Proxy @(ImmutableCR a)) x) acc
      acc' <- acc += r
      go acc' (i P.- 1)
  e <- empty @(ImmutableCR a)
  go e (min s n)

data NullableValue a = NullValue | ExistsValue { value :: a } deriving (Eq, Ord, Generic, Binary)

instance HasField "exists" (NullableValue a) Bool where
  getField ExistsValue{} = True
  getField NullValue = False

instance Show a => Show (NullableValue a) where
  show NullValue = "null"
  show ExistsValue{value} = show value


-- | Get a nullable value's value, or a default value.
--
-- @
-- elem <- randomElem'(c)
-- if (elem `orDefault` 1 > 0) do
--    ...
-- @
orDefault :: NullableValue a -> a -> a
orDefault NullValue d = d
orDefault ExistsValue{value} _d = value

--------------------------------------------------------------------------------
-- * Utilities

type family UnliftedS a where
  UnliftedS (System a) = a
  UnliftedS (Mutable a) = a  -- In certain instances, we'll require UnliftedS a ~ a because nested references are not supported.
  UnliftedS a = a

-- | Turn any value into a System one by automatically @return@ing it if
-- needed. The lifted value is @b@.
class (b ~ UnliftedS a) => LiftS a b where
  -- | The value that is lifted in(to) the system.
  liftS :: a -> System (UnliftedS a)

instance LiftS (Mutable a) a where
  liftS = get
  {-# INLINE liftS #-}

instance LiftS (System a) a where
  liftS = id
  -- we could check typeable for a = Mutable and get the value here, if anyone
  -- ever wanted to add together a System (Mutable Int) and an Int...
  {-# INLINE liftS #-}

instance (a ~ UnliftedS a) => LiftS a a where
  liftS = pure
  {-# INLINE liftS #-}

instance (Read a, Ord a, Typeable a) => ParseRecord (Set a) where
  parseRecord = fmap getOnly parseRecord

instance (Read a, Ord a, Typeable a) => ParseFields (Set a) -- default impl uses ParseField
instance (Read a, Ord a, Typeable a) => ParseField (Set a) where
  parseField hl fl sn dv =
    (\s -> S.fromList $
      fromMaybe (error $ "Failed to read from the command line option " ++ show fl ++ " a " ++ show (typeRep @a) ++ " in a comma separated set of values.")
      . readMaybe @a . T.unpack <$> T.split (P.== ',') s)
    <$> parseField @T.Text hl fl sn dv

