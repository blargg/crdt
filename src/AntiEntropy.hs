{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module AntiEntropy ( emptyState
                   , initialState
                   , runNode
                   ) where

import           Control.Concurrent          (forkIO, threadDelay)
import           Control.Concurrent.STM.TVar
import           Control.Exception
import           Control.Monad               (forever, unless, void)
import           Control.Monad.STM
import           Data.Maybe                  (fromMaybe)
import           Data.Serialize
import           GHC.Generics
import           Network.Socket
import           NetworkUtil

import           Data.Proxy
import qualified Network.Socket.ByteString   as NB

import           System.Log.Logger

import           Algebra.LesserAction
import           Algebra.SemiLatticeAction
import           Data.MultiAckSet

type MessageId = Int

data Message a = Payload a MessageId
               | Ack MessageId
               | Ping
                deriving (Generic)
instance (Serialize a) => Serialize (Message a)

data NodeState d a = NodeState { ackMap      :: MultiAckSet SockAddr MessageId d
                               , currentData :: a
                               }

emptyState :: a -> NodeState d a
emptyState = NodeState empty

initialState :: [(String, String)] -> a -> IO (NodeState d a)
initialState ns x = do
    rs <- getAddrs ns
    infoM "initialState" $ show rs
    return $ NodeState (setWithNeighbors rs) x

runNode :: (Show a, JoinLesserAction d a, Serialize d) => a -> PortNumber -> [String] -> IO (d -> IO (), IO a)
runNode x port ns = do
    cdrt <- newTVarIO =<< initialState (fmap splitAddress ns) x
    _ <- forkIO $ withSocketsDo $ bracket makeSocket close $ \sock -> do
        _ <- forkIO $ recieveNode cdrt sock
        updateNode cdrt sock
    return (addDelta cdrt, fmap currentData . atomically $ readTVar cdrt)
    where
        makeSocket = do
            (serveraddr:_) <- getAddrInfo (Just (defaultHints{addrFlags=[AI_PASSIVE]})) Nothing (Just (show port))
            sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
            bind sock (addrAddress serveraddr)
            return sock
        splitAddress :: String -> (String, String)
        splitAddress s = case break (== ':') s of
            (server,':':rs) -> (server, rs)
            i               -> i

recieveNode :: (JoinLesserAction d a, Serialize d, Show a) => TVar (NodeState d a) -> Socket -> IO ()
recieveNode state conn = do
    (msg, recvAddress) <- NB.recvFrom conn 1024
    debugM "server.handler" "received packet"
    case decode msg of
         Left str -> warningM "server.handler" str
         Right (Payload delta mID) -> handleDelta state conn recvAddress mID delta
         Right (Ack mID) -> receiveAck state recvAddress mID
         Right Ping -> atomically $ modifyTVar state (addNode recvAddress)
    recieveNode state conn

addDelta :: (JoinLesserAction d a) => TVar (NodeState d a) -> d -> IO ()
addDelta nodeState delta = do
    atomically $ do
        value <- currentData <$> readTVar nodeState
        unless (delta `joinLesserEq` value) $ modifyTVar nodeState updateState
    canAdd <- canMultiCast . ackMap <$> readTVarIO nodeState
    unless canAdd $ warningM "server.addDelta" "MultiAckSet ran out of fresh IDs, cannot broadcast new deltas"
    where updateState (NodeState ackM x) = NodeState (quietMultiCast delta ackM) (apply delta x)

quietMultiCast :: (Ord i) => a -> MultiAckSet n i a -> MultiAckSet n i a
quietMultiCast x ack = fromMaybe ack (multiCast x ack)

handleDelta :: forall d a. (JoinLesserAction d a, Serialize d) => TVar (NodeState d a) -> Socket -> SockAddr -> MessageId -> d -> IO ()
handleDelta nodeState s n i delta = do
    debugM "server.addDelta" $ "received delta from <" ++ show n ++ "> with message id " ++ show i
    addDelta nodeState delta
    ackDelta (Proxy :: Proxy d) s n i

receiveAck :: TVar (NodeState d a) -> SockAddr -> MessageId -> IO ()
receiveAck tns addr m = do
    atomically $ modifyTVar tns (updateAck addr m)
    debugM "server.receiveAck" $ "Received ack from <" ++ show addr ++ "> Message id " ++ show m

updateAck :: SockAddr -> MessageId -> NodeState d a -> NodeState d a
updateAck addr recievedMId ns = NodeState ackM' x
    where ackM' = singleAck addr recievedMId (ackMap ns)
          x = currentData ns

addNode :: SockAddr -> NodeState d a -> NodeState d a
addNode addr (NodeState ackM x) = NodeState ackM' x
    where ackM' = addNeighbor addr ackM

updateNode :: (Serialize d) => TVar (NodeState d a) -> Socket -> IO ()
updateNode tns sock = forever $ resendMessages tns sock >> threadDelay 1000000

resendMessages :: (Serialize d) => TVar (NodeState d a) -> Socket -> IO ()
resendMessages tns sock = do
    ns <- readTVarIO tns
    let messages = messagesToResend ns
    mapM_ (uncurry $ sendAddr sock) messages
    debugM "server.resendMessages" $ "resending " ++ show (length messages) ++ " messages"

messagesToResend :: NodeState d a -> [(SockAddr, Message d)]
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
