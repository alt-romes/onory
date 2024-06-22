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

  DeclareNotification
    :: Typeable nt => Maybe Name -> (Notification nt -> next) -> SystemF next

  UponNotification
    :: Notification nt -> (nt -> System ()) -> next -> SystemF next

  GetSelf
    :: (Host -> next) -> SystemF next

  MkNew
    :: a -> (Mutable a -> next) -> SystemF next

--------------------------------------------------------------------------------
-- Core datatypes

data Notification notification_t
  = Request Name
  | Indication Name
  | Message
  deriving Show

newtype Mutable a = Mutable (IORef a)

type Name = String
type Host = String

--------------------------------------------------------------------------------

-- I'll give you a functor ...
instance Functor SystemF where
  fmap f = \case
    DeclareNotification n c -> DeclareNotification n (f . c)
    UponNotification x impl next -> UponNotification x impl (f next)
    GetSelf c -> GetSelf (f . c)
    MkNew t c -> MkNew t (f . c)

-- Make me a monad... for free!
$(makeFree ''SystemF)

