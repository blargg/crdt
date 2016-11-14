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
import Data.MultiAckSet

type MessageId = Int

data Message a = Payload a MessageId | Ack MessageId deriving (Generic)
instance (Serialize a) => Serialize (Message a)

data NodeState a = NodeState { ackMap :: MultiAckSet SockAddr MessageId (Delta a)
                             , currentData :: a
                             }

initialState :: a -> NodeState a
initialState = NodeState empty

recieveNode :: (DCRDT a, Serialize (Delta a), Show a) => TVar (NodeState a) -> String -> IO ()
recieveNode state port = withSocketsDo $ bracket connectMe close (handler state)
    where
        connectMe = do
            (serveraddr:_) <- getAddrInfo (Just (defaultHints {addrFlags = [AI_PASSIVE]})) Nothing (Just port)
            sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
            bind sock (addrAddress serveraddr) >> return sock

handler :: (DCRDT a, Serialize (Delta a), Show a) => TVar (NodeState a) -> Socket -> IO ()
handler state conn = do
    (msg, recvAddress) <- NB.recvFrom conn 1024
    debugM "server.handler" $ "< " ++ unpack msg
    case decode msg of
         Left str -> warningM "server.handler" str
         Right (Payload delta mID) -> addDelta state recvAddress mID delta
         Right (Ack mID) -> receiveAck state recvAddress mID
    handler state conn

addDelta :: (DCRDT a) => TVar (NodeState a) -> SockAddr -> MessageId -> Delta a -> IO ()
addDelta nodeState n i delta = do
    atomically $ modifyTVar nodeState updateState
    debugM "server.addDelta" $ "received delta from <" ++ show n ++ "> with message id " ++ show i
    where updateState (NodeState ackM x) = NodeState (singleAck n i ackM) (apply delta x)

receiveAck :: TVar (NodeState a) -> SockAddr -> MessageId -> IO ()
receiveAck tns addr m = do
    atomically $ modifyTVar tns (updateAck addr m)
    debugM "server.receiveAck" $ "Received ack from <" ++ show addr ++ "> Message id " ++ show m

updateAck :: SockAddr -> MessageId -> NodeState a -> NodeState a
updateAck addr recievedMId (NodeState ackM x) = NodeState ackM' x
    where ackM' = singleAck addr recievedMId ackM

resendMessages :: (Serialize (Delta a)) => TVar (NodeState a) -> IO ()
resendMessages tns = do
    ns <- readTVarIO tns
    let messages = messagesToResend ns
    mapM_ (uncurry sendAddr) messages
    debugM "server.resendMessages" $ "resending " ++ show (length messages) ++ " messages"

messagesToResend :: NodeState a -> [(SockAddr, Message (Delta a))]
messagesToResend (NodeState ackM _) = (\(addr, mid, delta) -> (addr, Payload delta mid)) <$> listTakeMessages 10 ackM

ackDelta :: forall a. (Serialize a) => Proxy a -> Socket -> SockAddr -> MessageId -> IO ()
ackDelta _ conn otherAddress mId = void $ NB.sendTo conn message otherAddress
    where message = encode (Ack mId :: Message a)

sendAddr :: (Serialize a) => SockAddr -> a -> IO ()
sendAddr addr x = withSocketsDo $ bracket getSocket close talk where
    getSocket = do
        (toAddr:_) <- getAddrInfo (Just (defaultHints{addrAddress=addr})) Nothing Nothing
        s <- socket (addrFamily toAddr) Datagram defaultProtocol
        connect s (addrAddress toAddr)
        return s
    talk s = do
        _ <- NB.send s (encode x)
        debugM "client.talk" "sent a message"

simpleSend :: (Serialize a) => String -> String -> a -> IO ()
simpleSend ipAddr port delta = withSocketsDo $ bracket getSocket close talk where
    getSocket = do
        (serveraddr:_) <- getAddrInfo Nothing (Just ipAddr) (Just port)
        s <- socket (addrFamily serveraddr) Datagram defaultProtocol
        connect s (addrAddress serveraddr) >> return s
    talk s = do
        _ <- NB.send s (encode delta)
        debugM "simpleSend" "sent a message"
