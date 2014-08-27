module Protocol where

import Peer
import Request
import Response
import Hex
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy.Char8 as U
import qualified Data.ByteString.Lazy as B
import qualified Crypto.Hash.SHA256 as C
import Control.Concurrent

type Hash = U.ByteString

data Part = Part { part :: U.ByteString
                 , h    :: Hash }

data PeerState = 
    PeerState
        { m_peers :: Set.Set Peer
        , m_files :: Map.Map Hash [Part]
        }

ps_add_peers peers ps = do return $  ps { m_peers = Set.union (m_peers ps) peers }

get_file_part :: Hash -> Int -> PeerState -> Maybe Part
get_file_part h n ps = case Map.lookup h (m_files ps) of
    Just hps -> if n < length hps then Just $ hps !! n
                                  else Nothing
    Nothing -> Nothing


empty = PeerState Set.empty Map.empty



attendRequest :: MVar PeerState -> Request -> IO (Maybe Response)
attendRequest mvar (GetPeersRequest sender) = 
    do ps <- takeMVar mvar
       let ps' = ps { m_peers = Set.insert sender  (m_peers ps) }
       putMVar mvar ps'
       return $ Just $ GetPeersResponse (Set.toList $ m_peers ps')
       
attendRequest mvar (DownloadRequest h pn) = 
    do ps <- readMVar mvar
       case get_file_part (unhex h) pn ps of 
           Just (Part hp pp) -> return $ Just (DownloadResponse (hex hp) l pn (hex pp))
                where l = fromIntegral $ B.length pp
           Nothing -> return Nothing
       



