{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
module AntiEntropy where

import GHC.Generics
import Network.Socket
import Control.Monad (void)
import Control.Exception
import Control.Monad.STM
import Control.Concurrent.STM.TVar
import Data.Serialize

import qualified Network.Socket.ByteString as NB
import Data.ByteString.Char8 (unpack)
import Data.Proxy

import System.Log.Logger

import DeltaCRDT

type MessageId = Int

data Message a = Payload a MessageId | Ack MessageId deriving (Generic)
instance (Serialize a) => Serialize (Message a)

recieveNode :: (DCRDT a, Serialize (Delta a), Show a) => TVar a -> String -> IO ()
recieveNode cdrt port = withSocketsDo $ bracket connectMe close (handler cdrt)
    where
        connectMe = do
            (serveraddr:_) <- getAddrInfo (Just (defaultHints {addrFlags = [AI_PASSIVE]})) Nothing (Just port)
            sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
            bind sock (addrAddress serveraddr) >> return sock

handler :: (DCRDT a, Serialize (Delta a), Show a) => TVar a -> Socket -> IO ()
handler cdrt conn = do
    debugM "server.handler" "starting handler"
    (msg, _) <- NB.recvFrom conn 1024
    putStrLn $ "< " ++ unpack msg
    case decode msg of
         Left str -> infoM "server.handler" str
         Right delta -> addDelta cdrt delta
    atomically (readTVar cdrt) >>= print
    handler cdrt conn

addDelta :: (DCRDT a) => TVar a -> Delta a -> IO ()
addDelta cdrt delta = atomically $ modifyTVar cdrt (apply delta)

ackDelta :: forall a. (Serialize a) => Proxy a -> Socket -> SockAddr -> MessageId -> IO ()
ackDelta _ conn otherAddress mId = void $ NB.sendTo conn message otherAddress
    where message = encode (Ack mId :: Message a)

sendDelta :: (Serialize (Delta a)) => String -> String -> Delta a -> IO ()
sendDelta ipAddr port delta = withSocketsDo $ bracket getSocket close talk where
    getSocket = do
        (serveraddr:_) <- getAddrInfo Nothing (Just ipAddr) (Just port)
        s <- socket (addrFamily serveraddr) Datagram defaultProtocol
        connect s (addrAddress serveraddr) >> return s
    talk s = do
        _ <- NB.send s (encode delta)
        debugM "client.talk" "sent a message"
