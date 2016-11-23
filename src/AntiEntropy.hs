{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
module AntiEntropy where

import GHC.Generics
import Network.Socket
import NetworkUtil
import Control.Monad (void, forever, unless)
import Control.Exception
import Control.Monad.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent (forkIO, threadDelay)
import Data.Serialize

import qualified Network.Socket.ByteString as NB
import Data.ByteString.Char8 (unpack)
import Data.Proxy

import System.Log.Logger

import DeltaCRDT
import Data.MultiAckSet

type MessageId = Int

data Message a = Payload a MessageId
               | Ack MessageId
               | Ping
                deriving (Generic)
instance (Serialize a) => Serialize (Message a)

data NodeState a = NodeState { ackMap :: MultiAckSet SockAddr MessageId (Delta a)
                             , currentData :: a
                             }

emptyState :: a -> NodeState a
emptyState = NodeState empty

initialState :: [(String, String)] -> a -> IO (NodeState a)
initialState ns x = do
    rs <- getAddrs ns
    infoM "initialState" $ show rs
    return $ NodeState (setWithNeighbors rs) x

runNode :: (Show a, DCRDT' a, Serialize (Delta a)) => a -> PortNumber -> [String] -> IO ()
runNode x port ns = do
    cdrt <- newTVarIO =<< initialState (fmap splitAddress ns) x
    withSocketsDo $ bracket makeSocket close $ \sock -> do
        _ <- forkIO $ recieveNode cdrt sock
        _ <- forkIO $ updateNode cdrt sock
        forever $ threadDelay 99999999
    where
        makeSocket = do
            (serveraddr:_) <- getAddrInfo (Just (defaultHints{addrFlags=[AI_PASSIVE]})) Nothing (Just (show port))
            sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
            bind sock (addrAddress serveraddr)
            return sock
        splitAddress :: String -> (String, String)
        splitAddress s = case break (== ':') s of
            (server,':':rs) -> (server, rs)
            i -> i

recieveNode :: (DCRDT' a, Serialize (Delta a), Show a) => TVar (NodeState a) -> Socket -> IO ()
recieveNode state conn = do
    (msg, recvAddress) <- NB.recvFrom conn 1024
    debugM "server.handler" $ "< " ++ unpack msg
    case decode msg of
         Left str -> warningM "server.handler" str
         Right (Payload delta mID) -> addDelta state conn recvAddress mID delta
         Right (Ack mID) -> receiveAck state recvAddress mID
         Right Ping -> atomically $ modifyTVar state (addNode recvAddress)
    recieveNode state conn

addDelta :: forall a. (DCRDT' a, Serialize (Delta a)) => TVar (NodeState a) -> Socket -> SockAddr -> MessageId -> Delta a -> IO ()
addDelta nodeState s n i delta = do
    debugM "server.addDelta" $ "received delta from <" ++ show n ++ "> with message id " ++ show i
    atomically $ do
        value <- currentData <$> readTVar nodeState
        unless (isIdempotent delta value) $ modifyTVar nodeState updateState
    ackDelta (Proxy :: Proxy (Delta a)) s n i
    where updateState (NodeState ackM x) = NodeState (multiCast delta ackM) (apply delta x)

receiveAck :: TVar (NodeState a) -> SockAddr -> MessageId -> IO ()
receiveAck tns addr m = do
    atomically $ modifyTVar tns (updateAck addr m)
    debugM "server.receiveAck" $ "Received ack from <" ++ show addr ++ "> Message id " ++ show m

updateAck :: SockAddr -> MessageId -> NodeState a -> NodeState a
updateAck addr recievedMId (NodeState ackM x) = NodeState ackM' x
    where ackM' = singleAck addr recievedMId ackM

addNode :: SockAddr -> NodeState a -> NodeState a
addNode addr (NodeState ackM x) = NodeState ackM' x
    where ackM' = addNeighbor addr ackM

updateNode :: (Serialize (Delta a)) => TVar (NodeState a) -> Socket -> IO ()
updateNode tns sock = forever $ resendMessages tns sock >> threadDelay 1000000

resendMessages :: (Serialize (Delta a)) => TVar (NodeState a) -> Socket -> IO ()
resendMessages tns sock = do
    ns <- readTVarIO tns
    let messages = messagesToResend ns
    mapM_ (uncurry $ sendAddr sock) messages
    debugM "server.resendMessages" $ "resending " ++ show (length messages) ++ " messages"

messagesToResend :: NodeState a -> [(SockAddr, Message (Delta a))]
messagesToResend (NodeState ackM _) = (\(addr, mid, delta) -> (addr, Payload delta mid)) <$> listTakeMessages 10 ackM

ackDelta :: forall a. (Serialize a) => Proxy a -> Socket -> SockAddr -> MessageId -> IO ()
ackDelta _ conn otherAddress mId = do
    debugM "ackDelta" $ "ack message id " ++ show mId ++ " to address " ++ show otherAddress
    void $ NB.sendTo conn message otherAddress
    where message = encode (Ack mId :: Message a)

sendAddr :: (Serialize a) => Socket -> SockAddr -> a -> IO ()
sendAddr sock addr x = do
            _ <- NB.sendTo sock (encode x) addr
            debugM "client.talk" "sent a message"

simpleSend :: (Serialize a) => PortNumber -> String -> String -> a -> IO ()
simpleSend localPort ipAddr port delta = withSocketsDo $ bracket getSocket close talk where
    getSocket = do
        (serveraddr:_) <- getAddrInfo Nothing (Just ipAddr) (Just port)
        debugM "simpleSend" $ "server addr " ++ show serveraddr
        s <- socket (addrFamily serveraddr) Datagram defaultProtocol
        bind s (SockAddrInet localPort iNADDR_ANY)
        connect s (addrAddress serveraddr)
        return s
    talk s = do
        _ <- NB.send s (encode delta)
        debugM "simpleSend" "sent a message"
