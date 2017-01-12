module NetworkUtil where

import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe
import           Data.Maybe
import           Network.Socket

getAddr :: String -> String -> MaybeT IO SockAddr
getAddr ipAddr port = do
        addrs <- lift $ getAddrInfo (Just defaultHints {addrFamily = AF_INET}) (Just ipAddr) (Just port)
        case addrs of
            serveraddr : _ -> return $ addrAddress serveraddr
            _              -> MaybeT (return Nothing)

getAddrs :: [(String, String)] -> IO [SockAddr]
getAddrs ls = catMaybes <$> mapM (runMaybeT . uncurry getAddr) ls
