{-# LANGUAGE TemplateHaskell, GADTs, DataKinds, LambdaCase #-}
module System.Spec.Free where

import Control.Concurrent.STM
import Data.IORef
import Data.Kind
import Type.Reflection

import Control.Monad.Free
import Control.Monad.Free.TH

-- | A distributed system specification
type System = Free SystemF

-- | Not /that/ SystemF.
--
-- A functor to generate the embedded language of distributed systems specifications.
data SystemF next where

  UponEvent
    :: Event et -> (et -> System a) -> next -> SystemF next

  TriggerEvent
    :: Event et -> et -> next -> SystemF next

  GetSelf
    :: (Host -> next) -> SystemF next

  MkNew
    :: a -> (Mutable a -> next) -> SystemF next

  ModifyState
    :: Mutable a -> (a -> a) -> next -> SystemF next

  GetState
    :: Mutable a -> (a -> next) -> SystemF next

--------------------------------------------------------------------------------
-- Core datatypes

data Event evt_t
  = Request    { name :: Name, argTy :: TypeRep evt_t }
  | Indication { name :: Name, argTy :: TypeRep evt_t }
  | Message    { argTy :: TypeRep evt_t }
  | Timer      { argTy :: TypeRep evt_t }
  deriving (Show, Eq, Ord)

newtype Mutable a = Mutable (IORef a)

type Name = String
type Host = String

data TimerType
  = PeriodicTimer
  | OneShotTimer

--------------------------------------------------------------------------------

-- I'll give you a functor ...
instance Functor SystemF where
  fmap f = \case
    UponEvent x impl next -> UponEvent x impl (f next)
    TriggerEvent x e n -> TriggerEvent x e (f n)
    GetSelf c -> GetSelf (f . c)
    MkNew t c -> MkNew t (f . c)
    ModifyState a b n -> ModifyState a b (f n)
    GetState a n -> GetState a (f . n)

-- Make me a monad... for free!
$(makeFree ''SystemF)

