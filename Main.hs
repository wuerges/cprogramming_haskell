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
import Fileio

server_ip = "127.0.0.1" 
my_hash = "my_hash_1"

fileDownload hash = do
    

client mvar = do
    threadDelay 5000000
    requestPeers mvar 
    client mvar

--server mvar peer fs = performNetworkService (handler mvar) peer

handler :: MVar PeerState -> Socket -> IO ()
handler mvar conn = do
    (req,d) <- recvFrom conn (read_max)
    rsp <- attendRequestBS mvar req
    sendTo conn rsp d
    handler mvar conn

mainloop dir port peers = do
    fs <- loadFiles dir
    mvar <- newMVar $ mkPSFM fs
    -- Adding myself to the list of known peers
    let my_peer = Peer (T.pack server_ip) port (T.pack my_hash)

    modifyMVar_ mvar (ps_add_peer my_peer)
    modifyMVar_ mvar (ps_add_peersL peers)

    --forkIO (server mvar my_peer fs)
    forkIO $ performNetworkService (handler mvar) my_peer
    threadDelay 1000000
    client mvar
    return ()

main = do
    dir : port : peers : [] <- getArgs
    mainloop dir (read port) (decodePeersS peers)
