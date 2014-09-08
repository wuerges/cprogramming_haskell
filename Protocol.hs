module Protocol where

import Peer
import Request
import Response
import Hex
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy.Char8 as U
import qualified Data.ByteString.Char8 as SU
import qualified Data.ByteString.Lazy as B
import qualified Crypto.Hash.SHA256 as C
import Control.Concurrent
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import Control.Exception
import Data.Maybe
import Debug.Trace
import Fileio

catchNetwork :: a -> IO a -> IO a
catchNetwork df f = catch f (\ x -> do let err = show (x :: IOException)
                                       putStrLn $ "Got exception: " ++ err 
                                       return df)

data PeerState = 
    PeerState
        { m_peers :: Set.Set Peer
        , m_files :: FileMap
        }

read_max = (16*1024)

tr x = traceShow x x


ps_add_peer peer ps = do return $ ps { m_peers = Set.insert peer (m_peers ps) }
ps_add_peers peers ps = do return $ ps { m_peers = Set.union (m_peers ps) peers }
ps_add_peersL peers ps = ps_add_peers (Set.fromList peers) ps


mkPSFM fm = PeerState Set.empty fm
--empty = PeerState Set.empty Map.empty


attendRequestBS :: MVar PeerState -> SU.ByteString -> IO SU.ByteString 
attendRequestBS mvar msg = do
    case Request.decode msg of 
        Just req -> do rsp <- attendRequest mvar req
                       case rsp of
                           Just rsp -> return (Response.encode rsp)
                           Nothing -> do putStrLn "could not attend request"
                                         return SU.empty
        Nothing -> do putStrLn "could not decode request"
                      return SU.empty

attendRequest :: MVar PeerState -> Request -> IO (Maybe Response)
attendRequest mvar (GetPeersRequest sender) = 
    do ps <- readMVar mvar
       let ps' = ps { m_peers = Set.insert sender  (m_peers ps) }
       modifyMVar_ mvar (\ps -> return ps')
       return $ Just $ GetPeersResponse (Set.toList $ m_peers ps')
       
attendRequest mvar (DownloadRequest h pn) = 
    do ps <- readMVar mvar
       case getFilePart (Hash $ unhex h) pn (m_files ps) of 
           Just (Part pp (Hash hp), num_parts) -> return $ tr $ Just (DownloadResponse (hex hp) l pn num_parts (hex pp))
                where l = fromIntegral $ B.length pp
           Nothing -> return Nothing
       
performNetwork :: (Socket -> IO a) -> Peer -> IO a
performNetwork t peer = withSocketsDo $ bracket getSocket sClose t
    where getSocket = do
            (serveraddr:_) <- getAddrInfo Nothing (Just $ ipS peer) (Just $ portS peer)
            s <- socket (addrFamily serveraddr) Datagram defaultProtocol
            connect s (addrAddress serveraddr)
            return s

performNetworkService :: (Socket -> IO a) -> Peer -> IO a
performNetworkService t peer = withSocketsDo $ bracket connectMe sClose t
    where connectMe = do
            (serveraddr:_) <- getAddrInfo
                                (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                                Nothing (Just $ portS peer)
            sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
            bindSocket sock (addrAddress serveraddr) >> return sock

requestPeers1 :: Peer -> IO (Set.Set Peer)
requestPeers1 peer = do catchNetwork Set.empty $ performNetwork talk peer
    where talk s = do
          send s $ Request.encode $ GetPeersRequest peer 
          msg <- recv s read_max
          return $ Set.fromList $ peers $ fromMaybe null_peers_response (Response.decode msg)

ts x = traceShow x x

requestPeers mvar = do
    ps <- readMVar mvar 
    peers <- mapM requestPeers1 (Set.toList $ ts $ m_peers ps)
    modifyMVar_ mvar (ps_add_peers (Set.unions peers))

requestPart :: Peer -> Hash -> Int -> IO (Maybe Part, Int)
requestPart peer (Hash h) n = do catchNetwork (Nothing, 0) $ performNetwork talk peer
    where talk s = do
          send s $ Request.encode $ DownloadRequest (hex h) n
          msg <- recv s read_max
          case Response.decode msg of 
                Just (DownloadResponse h' l' n' n_o_p' p') -> return (Just $ Part  (unhex p') (Hash $ unhex h'), n_o_p')
                Nothing -> return (Nothing, 0)

