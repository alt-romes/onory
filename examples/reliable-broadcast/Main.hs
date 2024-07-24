import BestEffortBroadcast
import ReliableBroadcast
import Data.Proxy
import System.Distributed.Interpret

main = runProtos
  [ P (bestEffortBroadcast (Proxy @BebPayload))
  , P reliableBroadcast
  ]
