import AntiEntropy
import DeltaCRDT
import Algebra.Lattice.Ordered
import Control.Monad (forever)
import Control.Concurrent.STM.TVar
import Control.Concurrent (forkIO, threadDelay)

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
         "server":port:ns -> runServer port ns
         "client":param:_ -> runClient (read param)
         _ -> putStrLn "run as server or client <integer>"

runServer :: String -> [String]-> IO ()
runServer port ns = do
    cdrt <- newTVarIO =<< initialState (fmap splitAddress ns) (Ordered 0:: Ordered Int)
    _ <- forkIO $ recieveNode cdrt port
    _ <- forkIO $ updateNode cdrt (read port)
    forever $ threadDelay 99999999

splitAddress :: String -> (String, String)
splitAddress s = case break (== ':') s of
    (server,':':rs) -> (server, rs)
    x -> x

runClient :: Int -> IO ()
runClient value = simpleSend 9876 localIP serverPort (Payload (DeltaOrdered value) 1)
