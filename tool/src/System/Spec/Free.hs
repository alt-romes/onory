{-# LANGUAGE TemplateHaskell, GADTs, DataKinds, LambdaCase #-}
module System.Spec.Free where

import GHC.Records
import Control.Concurrent.STM
import Data.IORef
import Data.Kind
import Type.Reflection
import System.Random (Random)

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

  GetRandom
    :: Random a => (a, a) -> (a -> next) -> SystemF next

  SetupTimer
    :: HasField "time" timer Int => TimerType timer -> Event timer -> timer -> next -> SystemF next

  CancelTimer
    :: Event timer -> next -> SystemF next

  TraceStr
    :: Int {- verbosity -} -> String -> next -> SystemF next

--------------------------------------------------------------------------------
-- Core datatypes

data Event (evt_t :: Type)
  = Request    { name :: Name, argTy :: TypeRep evt_t }
  | Indication { name :: Name, argTy :: TypeRep evt_t }
  | Message    { argTy :: TypeRep evt_t }
  | Timer      { argTy :: TypeRep evt_t }
  | StopTimer  { argTy :: TypeRep evt_t }
  deriving (Eq, Ord)

newtype Mutable a = Mutable (IORef a)

type Name = String
type Host = String

data TimerType timer where
  PeriodicTimer :: HasField "repeat" timer Int => TimerType timer
  OneShotTimer  :: TimerType timer

instance Show (Event t) where
  show e = case e of
    Request{name, argTy}    -> "request "      ++ show name ++ ":" ++ show argTy
    Indication{name, argTy} -> "indication "   ++ show name ++ ":" ++ show argTy
    Message{argTy}          -> "message "      ++ show argTy
    Timer{argTy}            -> "timer "        ++ show argTy
    StopTimer{argTy}        -> "cancel timer " ++ show argTy

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
    GetRandom r n -> GetRandom r (f . n)
    SetupTimer tt evt timer next -> SetupTimer tt evt timer (f next)
    CancelTimer evt n -> CancelTimer evt (f n)
    TraceStr i s n -> TraceStr i s (f n)

-- Make me a monad... for free!
$(makeFree ''SystemF)

