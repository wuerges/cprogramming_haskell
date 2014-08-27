import Protocol
import Request
import Response
import Peer
import qualified Data.Set as Set
import Data.Maybe
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import Control.Exception
import Control.Monad (unless)
import Control.Monad.Trans 
import Control.Monad.State.Lazy
import Control.Monad.Identity
import Control.Concurrent
import Debug.Trace
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as U
import qualified Data.ByteString.Char8 as SU

server_port = 3006
server_ip = "127.0.0.1" 
my_hash = "my_hash_1"

read_max = (16*1024)

requestPeers1 :: Socket -> Peer -> IO (Set.Set Peer)
requestPeers1 s peer = do
    send s $ Request.encode $ GetPeersRequest peer 
    msg <- recv s read_max
    return $ Set.fromList $ peers $ fromMaybe null_peers_response (Response.decode msg)

ts x = traceShow x x

requestPeers mvar s = do
    ps <- readMVar mvar 
    peers <- mapM (requestPeers1 s) (Set.toList $ ts $ m_peers ps)
    modifyMVar_ mvar (ps_add_peers (Set.unions peers))

talk :: MVar PeerState -> Socket -> IO ()
talk mvar s = do
    modifyMVar_ mvar (ps_add_peers (Set.fromList [Peer (T.pack server_ip) server_port (T.pack my_hash)]))
    requestPeers mvar s
    threadDelay 5000000
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
    case Request.decode msg of 
        Just req -> do rsp <- attendRequest mvar req
                       case rsp of
                            Just rsp -> sendTo conn (Response.encode rsp) d
                            Nothing -> do putStrLn "could not attend request"
                                          return 0
        Nothing -> do putStrLn "could not decode request"
                      return 0
    handler mvar conn

main = do
    mvar <- newMVar Protocol.empty
    forkIO (server mvar)
    threadDelay 1000
    client mvar
