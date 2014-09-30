import Protocol
import Control.Exception
import Request
import Response
import Peer
import qualified Data.Set as Set
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import Control.Concurrent
import Data.Maybe
import System.Environment
import qualified Data.Text as T
import Fileio
import DHT

my_hash = "03030103000102030001020300010203"

fileDownload hash peer = do
    threadDelay 5000
    rsp <- requestPart peer hash 0
    case rsp of 
       (Just p0, l) -> do pnts <- mapM (requestPart peer hash) [1..l]
                          let pns = catMaybes $ map fst pnts
                          putStrLn "Downloaded file completely"
                          let f = File hash (p0 : pns)
                          putStrLn $ show f 
                          return $ Just f
       (Nothing, _) -> return Nothing

offerClient my_peer mvar = do
    threadDelay 10000000
    offerFiles my_peer mvar 
    offerClient my_peer mvar

client my_peer mvar = do
    threadDelay 5000000
    requestPeers my_peer mvar 
    client my_peer mvar

mainloop dir port server_ip peers = do
    fs <- loadFiles dir
    mvar <- newMVar $ mkPSFM fs (simplifyHash $ T.pack $ my_hash)
    -- Adding myself to the list of known peers
    let my_peer = Peer (T.pack server_ip) port (T.pack my_hash)

    modifyMVar_ mvar (ps_add_peersIO $ Set.fromList (my_peer:peers))

    --forkIO (server mvar my_peer fs)
        
    forkIO $ catchNetwork () $ performNetworkService (handler mvar) my_peer
    threadDelay 1000000

    --fileDownload (Fileio.hashes fs !! 1) my_peer

    --forkIO $ offerClient my_peer mvar
    client my_peer mvar
    return ()

main = do
    dir : port : server_ip : peers : [] <- getArgs
    mainloop dir (read port) server_ip  (decodePeersS peers)
