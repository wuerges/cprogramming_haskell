import Protocol
import Request
import Response
import Peer
import qualified Data.Set as Set
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import Control.Concurrent
import System.Environment
import qualified Data.Text as T

server_ip = "127.0.0.1" 
my_hash = "my_hash_1"


client mvar = do
    threadDelay 5000000
    requestPeers mvar 
    client mvar

{-
server mvar peer = withSocketsDo $ bracket connectMe sClose (handler mvar)
    where connectMe = do
            (serveraddr:_) <- getAddrInfo
                                (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                                Nothing (Just $ portS peer)
            sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
            bindSocket sock (addrAddress serveraddr) >> return sock
            -}

server mvar peer = performNetworkService (handler mvar) peer

handler :: MVar PeerState -> Socket -> IO ()
handler mvar conn = do
    (req,d) <- recvFrom conn (read_max)
    rsp <- attendRequestBS mvar req
    sendTo conn rsp d
    handler mvar conn

mainloop port peers = do
    mvar <- newMVar Protocol.empty
    -- Adding myself to the list of known peers
    let my_peer = Peer (T.pack server_ip) port (T.pack my_hash)

    modifyMVar_ mvar (ps_add_peer my_peer)
    modifyMVar_ mvar (ps_add_peersL peers)

    forkIO (server mvar my_peer)
    threadDelay 1000000
    client mvar
    return ()

main = do
    port : peers : [] <- getArgs
    mainloop (read port) (decodePeersS peers)
