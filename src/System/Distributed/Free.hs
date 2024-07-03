{-# LANGUAGE TemplateHaskell, GADTs, DataKinds, LambdaCase, DuplicateRecordFields, ViewPatterns #-}
module System.Distributed.Free where

import GHC.Fingerprint
import GHC.Records
import Data.Binary
import Data.IORef
import Data.Proxy
import Data.String
import Data.Kind
import Type.Reflection
import System.Random (Random)
import Network.Transport (EndPointAddress(..), TransportError, ConnectErrorCode)
import qualified Data.ByteString.Char8 as BS8
import Control.Monad.Free
-- import Control.Monad.Free.Church (TODO!!)
import Control.Monad.Free.TH
import Options.Generic

-- | A distributed system specification
type System = Free SystemF

-- | A protocol specifies the behaviour of a part of the distributed system.
-- Protocols running as part of the same system may interact with one another by
-- using requests and indications.
--
-- A protocol should never need to handle timers set-up by other protocols, nor
-- by messages declared, send and received by other protocols.
--
-- This can be reasonably ensured by defining one protocol per module, and
-- common indications and requests in a common module imported by all protocol
-- modules.
--
-- The main module should then be responsible for putting together all these
-- protocols and running them as a whole system (using an interpreter).
--
-- The boundary of the protocol must also be made explicit by using
-- 'protocolBoundary' (or 'protocol' from 'System.Distributed.Core'). Declaring
-- the boundary of the protocol allows it to be treated as a unit which can run
-- in parallel to other protocols.
type Protocol name = System (Proxy name)

-- | Not /that/ SystemF.
--
-- A functor to generate the embedded language of distributed systems specifications.
data SystemF next where

  ProtocolBoundary
    :: Name -> System a -> next -> SystemF next

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
    :: TimerType timer -> Event timer -> timer -> next -> SystemF next

  CancelTimer
    :: Event timer -> next -> SystemF next

  TraceStr
    :: Verbosity -> String -> next -> SystemF next

  EscapeTheSystem -- An escape hatch to do arbitrary IO within a system
    :: IO a -> (a -> next) -> SystemF next

--------------------------------------------------------------------------------
-- Core datatypes

data Event (evt_t :: Type) where
  Request
    :: { name :: String, argTy :: TypeRep evt_t } -> Event evt_t
  Indication
    :: { name :: String, argTy :: TypeRep evt_t } -> Event evt_t
  Message
    :: (HasField "to" evt_t Host, Binary evt_t)
    => { tyId :: Fingerprint, tyStr :: String }   -> Event evt_t
  Timer
    :: HasField "time" evt_t Int
    => { argTy :: TypeRep evt_t } -> Event evt_t
  StopTimer
    :: { argTy :: TypeRep evt_t } -> Event evt_t
  ChannelEvt
    :: { argTy :: TypeRep evt_t } -> Event evt_t

-- NB: Messages only store tyStr = @show (typeRep @evt_t)@ for tracing purposes.
-- The fingerprint is the sole field used for the event key ordering.

newtype Mutable a = Mutable (IORef a)

data TimerType timer where
  PeriodicTimer :: HasField "repeat" timer Int => TimerType timer
  OneShotTimer  :: TimerType timer

newtype Host = Host { addr :: EndPointAddress }

type Name = String
type Verbosity = Int

--------------------------------------------------------------------------------
-- Network channel events

newtype OutConnUp     = OutConnUp { to :: Host }
newtype OutConnDown   = OutConnDown { to :: Host }
data    OutConnFailed = OutConnFailed { to :: Host, err :: TransportError ConnectErrorCode }
newtype InConnUp      = InConnUp { from :: Host }
newtype InConnDown    = InConnDown { from :: Host }

--------------------------------------------------------------------------------

-- I'll give you a functor ...
instance Functor SystemF where
  fmap f = \case
    ProtocolBoundary n p next -> ProtocolBoundary n p (f next)
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
    EscapeTheSystem io n -> EscapeTheSystem io (f . n)

-- Make me a monad... for free!
$(makeFree ''SystemF)

--------------------------------------------------------------------------------
-- Instances and utilities

instance Show (Event t) where
  show e = case e of
    Request{name, argTy}    -> "request "      ++ name ++ "@" ++ show argTy
    Indication{name, argTy} -> "indication "   ++ name ++ "@" ++ show argTy
    Message{tyStr, tyId=_}  -> "message "      ++ tyStr
    Timer{argTy}            -> "timer "        ++ show argTy
    StopTimer{argTy}        -> "cancel timer " ++ show argTy
    ChannelEvt{argTy}       -> "channel "      ++ show argTy

deriving instance Eq Host
deriving instance Ord Host
deriving instance Binary Host

instance Show Host where
  show (Host (EndPointAddress (reverse . BS8.unpack -> '0':':':xs))) = reverse xs
  show _ = error "Internal error: expecting all endpoint addresses to have endpoint id = 0"
    -- we cutoff the ':0' bit, an internal detail.

instance IsString Host where
  fromString = Host . EndPointAddress . fromString . (++ ":0")
    -- invariant: the nodes always use a single endpoint no 0

instance ParseRecord Host where
  parseRecord = fmap getOnly parseRecord

instance ParseFields Host where
  parseFields hm fl sn dv = fromString <$> parseFields hm fl sn dv
