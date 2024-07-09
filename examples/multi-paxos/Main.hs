{-# LANGUAGE ExplicitNamespaces, PatternSynonyms #-}
module Main where

import Paxos
import System.Distributed.Interpret
import System.Distributed.Prelude (Int, pattern Map)

main = runProtos [P (paxos @Int Map)]

