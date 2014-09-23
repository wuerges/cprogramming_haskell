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
import DHT

ts x = traceShow x x

catchNetwork :: a -> IO a -> IO a
catchNetwork df f = catch f (\ x -> do let err = show (x :: IOException)
                                       putStrLn $ "Got exception: " ++ err 
                                       return df)

data PeerState = 
    PeerState
        { m_peers :: Set.Set Peer
        , m_files :: FileMap
        , m_dht :: DHT Peer
        , m_dht_files :: DHT Peer
        }

read_max = (16*1024)

ps_add_peer peer ps = ps { m_peers = Set.insert peer (m_peers ps) 
                         , m_dht = addItemDHT i (m_dht ps)
                         }
              where i = Item (simplifyHash $ Peer.hash peer) (Value peer)

ps_add_peers :: Set.Set Peer -> PeerState -> PeerState
ps_add_peers peers ps = Set.fold ps_add_peer ps peers 

ps_add_peersIO peers ps = do return $ ps_add_peers peers ps

mkPSFM fm my_hash = PeerState Set.empty fm (genEmptyDHT my_hash) (genEmptyDHT my_hash)
--empty = PeerState Set.empty Map.empty

handler :: MVar PeerState -> Socket -> IO ()
handler mvar conn = do
    (req,d) <- recvFrom conn (read_max)
    forkIO $ do rsp <- attendRequestBS mvar req
                if SU.null rsp then return () 
                               else do sendTo conn rsp d
                                       return ()
    handler mvar conn

attendRequestBS :: MVar PeerState -> SU.ByteString -> IO SU.ByteString 
attendRequestBS mvar msg = do
    case Request.decode $ ts msg of 
        Just req -> do rsp <- attendRequest mvar (ts req)
                       case rsp of
                           Just LocalResponse -> return SU.empty
                           Just rsp -> return (Response.encode rsp)
                           Nothing -> do putStrLn "could not attend request"
                                         return SU.empty
        Nothing -> do putStrLn "could not decode request"
                      return SU.empty

attendRequest :: MVar PeerState -> Request -> IO (Maybe Response)
attendRequest mvar (OfferFile op fh) = 
    do ps <- readMVar mvar
       let ps' = ps { m_dht_files = addItemDHT (mkItemDHT fh op) (m_dht_files ps) } 
       modifyMVar_ mvar (\ps -> return ps')
       --return $ ts $ m_dht_files ps'
       return $ Just (trace ("attedend offer " ++ show (m_dht_files ps') ) LocalResponse)
       
attendRequest mvar (GetPeersRequest sender) = 
    do modifyMVar_ mvar (ps_add_peersIO $ Set.singleton sender)
       ps <- readMVar mvar
       return $ Just $ GetPeersResponse (Set.toList $ m_peers ps)
       
attendRequest mvar (DownloadRequest h pn) = 
    do ps <- readMVar mvar
       case getFilePart (Hash $ unhex $ h) pn (m_files ps) of 
           Just (Part pp (Hash hp), num_parts) -> return $ Just (DownloadResponse (hex hp) l pn num_parts (hex pp))
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

offerFiles1 :: Peer -> [Hash] -> Peer -> IO ()
offerFiles1 my_peer hs peer = do catchNetwork () $ performNetwork talk peer
    where talk s = do
          let f h = send s $ Request.encode $ OfferFile my_peer (hashToText h)
          mapM_ f hs
          return ()

offerFiles :: Peer -> MVar PeerState -> IO ()
offerFiles my_peer mvar = do
    ps <- readMVar mvar
    let peers = Set.toList $ m_peers ps
    mapM_ (offerFiles1 my_peer (hashes $ m_files ps)) peers


requestPeers1 :: Peer -> IO (Set.Set Peer)
requestPeers1 peer = do catchNetwork Set.empty $ performNetwork talk peer
    where talk s = do
          send s $ Request.encode $ GetPeersRequest peer 
          msg <- recv s read_max
          return $ Set.fromList $ peers $ fromMaybe null_peers_response (Response.decode $ msg)

requestPeers mvar = do
    ps <- readMVar mvar 
    peers <- mapM requestPeers1 (Set.toList $ m_peers ps)
    modifyMVar_ mvar (ps_add_peersIO (Set.unions peers))

requestPart :: Peer -> Hash -> Int -> IO (Maybe Part, Int)
requestPart peer (Hash h) n = do catchNetwork (Nothing, 0) $ performNetwork talk peer
    where talk s = do
          send s $ Request.encode $ DownloadRequest (hex h) n
          msg <- recv s read_max
          case Response.decode msg of 
                Just (DownloadResponse h' l' n' n_o_p' p') -> return (Just $ Part  (unhex p') (Hash $ unhex h'), n_o_p')
                Nothing -> return (Nothing, 0)

