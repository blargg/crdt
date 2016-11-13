import AntiEntropy
import Control.Concurrent.STM.TVar
import System.Environment (getArgs)
import DeltaCRDT
import Algebra.Lattice.Ordered

localIP :: String
localIP = "127.0.0.1"

serverPort :: String
serverPort = "3333"

main :: IO ()
main = do
    args <- getArgs
    case args of
         "server":_ -> runServer
         "client":param:_ -> runClient (read param)
         _ -> putStrLn "run as server or client <integer>"

runServer :: IO ()
runServer = do
    cdrt <- newTVarIO . initialState $ (Ordered 0:: Ordered Int)
    recieveNode cdrt serverPort

runClient :: Int -> IO ()
runClient value = simpleSend localIP serverPort (Payload (DeltaOrdered value) 1)
