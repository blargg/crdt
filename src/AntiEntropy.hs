{-# LANGUAGE FlexibleContexts #-}
module AntiEntropy where

import Network.Socket
import Control.Exception
import Control.Monad.STM
import Control.Concurrent.STM.TVar
import Data.Serialize

import qualified Network.Socket.ByteString as NB
import Data.ByteString.Char8 (unpack)

import System.Log.Logger

import DeltaCRDT


recieveNode :: (DCRDT a, Serialize (Delta a), Show a) => TVar a -> String -> IO ()
recieveNode cdrt port = withSocketsDo $ bracket connectMe close (handler cdrt)
    where
        connectMe = do
            (serveraddr:_) <- getAddrInfo (Just (defaultHints {addrFlags = [AI_PASSIVE]})) Nothing (Just port)
            sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
            bind sock (addrAddress serveraddr) >> return sock

handler :: (DCRDT a, Serialize (Delta a), Show a) => TVar a -> Socket -> IO ()
handler cdrt conn = do
    print "starting handler"
    (msg,d) <- NB.recvFrom conn 1024
    putStrLn $ "< " ++ unpack msg
    case decode msg of
         Left str -> debugM "server.handler" str
         Right delta -> atomically $ modifyTVar' cdrt (apply delta)
    atomically (readTVar cdrt) >>= print
    debugM "server.handler" "read a message"
    NB.sendTo conn msg d >> handler cdrt conn


sendDelta :: (DCRDT a, Serialize (Delta a)) => String -> String -> (Delta a) -> IO ()
sendDelta ipAddr port delta = withSocketsDo $ bracket getSocket close talk where
    getSocket = do
        (serveraddr:_) <- getAddrInfo Nothing (Just ipAddr) (Just port)
        s <- socket (addrFamily serveraddr) Datagram defaultProtocol
        connect s (addrAddress serveraddr) >> return s
    talk s = do
        _ <- NB.send s (encode delta)
        debugM "client.talk" "sent a message"

