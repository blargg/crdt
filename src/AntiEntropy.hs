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
import Data.Map (Map, empty)

import qualified Network.Socket.ByteString as NB
import Data.ByteString.Char8 (unpack)
import Data.Proxy

import System.Log.Logger

import DeltaCRDT
import LabeledSet

type MessageId = Int

data Message a = Payload a MessageId | Ack MessageId deriving (Generic)
instance (Serialize a) => Serialize (Message a)

data NodeState a = NodeState { ackMap :: Map SockAddr MessageId
                             , deltaMap :: LabeledSet MessageId (Delta a)
                             , currentData :: a
                             }

initialState :: a -> NodeState a
initialState = NodeState empty (emptySet 0)

recieveNode :: (DCRDT a, Serialize (Delta a), Show a) => TVar (NodeState a) -> String -> IO ()
recieveNode state port = withSocketsDo $ bracket connectMe close (handler state)
    where
        connectMe = do
            (serveraddr:_) <- getAddrInfo (Just (defaultHints {addrFlags = [AI_PASSIVE]})) Nothing (Just port)
            sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
            bind sock (addrAddress serveraddr) >> return sock

handler :: (DCRDT a, Serialize (Delta a), Show a) => TVar (NodeState a) -> Socket -> IO ()
handler state conn = do
    debugM "server.handler" "starting handler"
    (msg, _) <- NB.recvFrom conn 1024
    putStrLn $ "< " ++ unpack msg
    case decode msg of
         Left str -> infoM "server.handler" str
         Right delta -> addDelta state delta
    handler state conn

addDelta :: (DCRDT a) => TVar (NodeState a) -> Delta a -> IO ()
addDelta nodeState delta = atomically $ modifyTVar nodeState updateState
    where updateState (NodeState ackM deltaM x) = NodeState ackM (fst $ addNext delta deltaM) (apply delta x)

receiveAck :: MessageId -> IO ()
receiveAck = undefined

receiveDelta :: (DCRDT a) => Delta a -> IO ()
receiveDelta = undefined

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
