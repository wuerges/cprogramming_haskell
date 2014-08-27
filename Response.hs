{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Response where

import Peer
import Data.Aeson
import Data.Text
import GHC.Generics
import qualified Data.ByteString.Lazy.Char8 as U
import qualified Data.ByteString.Char8 as SU

data Response = 
    DownloadResponse
        { part_hash    :: !Text
        , part_length :: Int 
        , part_number :: Int 
        , part :: !Text
        } 
    | GetPeersResponse
        { peers :: [Peer]
        } 
        deriving (Show, Generic, Eq)

null_peers_response = GetPeersResponse []

instance FromJSON Response
instance ToJSON Response

encode :: Response -> SU.ByteString
encode r = U.toStrict $ Data.Aeson.encode r

decode :: SU.ByteString -> Maybe Response
decode r = Data.Aeson.decode (U.fromStrict r) :: Maybe Response

