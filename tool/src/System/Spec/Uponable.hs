module System.Spec.Uponable where

import Type.Reflection
import Data.Kind
import Control.Monad.Free
import System.Spec.Free

class Uponable n where
  type UponableArgs (n :: Type) :: Type
  uponImpl :: n -> (UponableArgs n -> System ()) -> System ()

instance Uponable (Request UND req) where
  type UponableArgs (Request UND req) = req
  uponImpl = uponReq

instance Uponable (Indication t) where
  type UponableArgs (Indication t) = t
  uponImpl = uponInd
