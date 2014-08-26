import Protocol
import Request
import Response
import Peer
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import Control.Exception
import Control.Monad (unless)
import Control.Monad.Trans 
import Control.Monad.State.Lazy
import Control.Monad.Identity
import Control.Concurrent
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as U
import qualified Data.ByteString.Char8 as SU

server_port = 3006
server_ip = "127.0.0.1" 
read_max = (16*1024)

talk :: MVar PeerState -> Socket -> IO ()
talk mvar s = do
    send s $ Request.encode $ GetPeersRequest (Peer (T.pack server_ip) server_port (T.pack "aoeuaoeu") )
    recv s read_max >>= \msg -> putStrLn $ "Received " ++ (SU.unpack msg)
    talk mvar s

client mvar = withSocketsDo $ bracket getSocket sClose (talk mvar)
    where getSocket = do
            (serveraddr:_) <- getAddrInfo Nothing (Just server_ip) (Just $ show server_port)
            s <- socket (addrFamily serveraddr) Datagram defaultProtocol
            connect s (addrAddress serveraddr) >> return s

server mvar = withSocketsDo $ bracket connectMe sClose (handler mvar)
    where connectMe = do
            (serveraddr:_) <- getAddrInfo
                                (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                                Nothing (Just $ show  server_port)
            sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
            bindSocket sock (addrAddress serveraddr) >> return sock

handler :: MVar PeerState -> Socket -> IO ()
handler mvar conn = do
    (msg,d) <- recvFrom conn (read_max)
    putStrLn $ "< " ++ (SU.unpack msg)
    case Request.decode msg of 
        Just rsp -> do putStrLn $ "sending " ++ (show rsp)
                       sendTo conn (Response.encode rsp) d
        Nothing -> do putStrLn "failed"
                      return 0
    handler mvar conn

main = do
    mvar <- newMVar Protocol.empty
    forkIO (server mvar)
    threadDelay 1000
    client mvar
