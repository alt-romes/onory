module Main where

import System.Pseudo.Free

main :: IO ()
main = do
  putStrLn "Hello, System!"

data UUID
data Host
data Bytes

asystem :: System ()
asystem = do
  broadcastRequest <- request@(UUID, Host, Bytes)
  return ()

