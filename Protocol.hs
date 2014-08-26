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
import Control.Monad.State.Lazy

type Hash = U.ByteString

data Part = Part { part :: U.ByteString
                 , h    :: Hash }

data PeerState = 
    PeerState
        { m_peers :: Set.Set Peer
        , m_files :: Map.Map Hash [Part]
        }

get_file_part :: Hash -> Int -> PeerState -> Maybe Part
get_file_part h n ps = case Map.lookup h (m_files ps) of
    Just hps -> if n < length hps then Just $ hps !! n
                                  else Nothing
    Nothing -> Nothing

initialS = PeerState (Set.empty)

{-DownloadResponse
        { part_hash    :: !Text
        , part_length :: Int 
        , part_number :: Int 
        , part :: !Text
        } 
    -}

attendRequest :: Request -> State PeerState (Maybe Response)
attendRequest (GetPeersRequest sender) = 
    do ps <- get
       let ps' = ps { m_peers = Set.insert sender  (m_peers ps) }
       put ps'
       return $ Just $ GetPeersResponse (Set.toList $ m_peers ps')
       
attendRequest (DownloadRequest h pn) = 
    do ps <- get
       case get_file_part (unhex h) pn ps of 
           Just (Part hp pp) -> return $ Just (DownloadResponse (hex hp) l pn (hex pp))
                where l = fromIntegral $ B.length pp
           Nothing -> return Nothing
       



