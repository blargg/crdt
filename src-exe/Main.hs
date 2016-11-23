import AntiEntropy
import DeltaCRDT
import Algebra.Lattice.Ordered

import System.Environment (getArgs)
import System.Log.Logger

localIP :: String
localIP = "127.0.0.1"

serverPort :: String
serverPort = "3333"

main :: IO ()
main = do
    updateGlobalLogger "" (setLevel DEBUG)
    args <- getArgs
    case args of
         "server":port:ns -> runNode (Ordered (0::Int)) (read port) ns
         "client":param:_ -> runClient (read param)
         _ -> putStrLn "run as server or client <integer>"


runClient :: Int -> IO ()
runClient value = simpleSend 9876 localIP serverPort (Payload (DeltaOrdered value) 1)
