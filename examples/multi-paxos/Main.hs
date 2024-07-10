module Main where

import Paxos
import System.Distributed.Interpret
import System.Distributed.Prelude (mapFromList)
import Prelude

main = runProtos [P (paxos @Int (mapFromList
  [ ("inc", \s -> (s+1,"Result: " ++ show (s+1)))
  , ("dec", \s -> (s-1,"Result: " ++ show (s-1)))
  , ("square", \s -> (s*s,"Result: " ++ show (s*s)))
  , ("double", \s -> (s+s,"Result: " ++ show (s+s)))
  , ("half", \s -> (s `div` 2,"Result: " ++ show (s `div` s)))
  ]))]

