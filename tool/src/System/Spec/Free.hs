{-# LANGUAGE TemplateHaskell, GADTs, DataKinds, LambdaCase #-}
module System.Spec.Free where

import Data.IORef
import Data.Kind
import Type.Reflection

import Control.Monad.Free
import Control.Monad.Free.TH

-- | A distributed system specification
type System a = Free SystemF a

-- | Not /that/ SystemF.
--
-- An algebra to generate the embedded language of distributed systems specifications.
data SystemF next where

  DeclareEvent
    :: Typeable et => Event et -> (Event et -> next) -> SystemF next

  UponEvent
    :: Event et -> (et -> System ()) -> next -> SystemF next

  GetSelf
    :: (Host -> next) -> SystemF next

  MkNew
    :: a -> (Mutable a -> next) -> SystemF next

  ModifyState
    :: Mutable a -> (a -> a) -> next -> SystemF next

--------------------------------------------------------------------------------
-- Core datatypes

data Event evt_t
  = Request Name
  | Indication Name
  | Message (TypeRep evt_t)
  deriving Show

newtype Mutable a = Mutable (IORef a)

type Name = String
type Host = String

--------------------------------------------------------------------------------

-- I'll give you a functor ...
instance Functor SystemF where
  fmap f = \case
    DeclareEvent n c -> DeclareEvent n (f . c)
    UponEvent x impl next -> UponEvent x impl (f next)
    GetSelf c -> GetSelf (f . c)
    MkNew t c -> MkNew t (f . c)
    ModifyState a b n -> ModifyState a b (f n)

-- Make me a monad... for free!
$(makeFree ''SystemF)

