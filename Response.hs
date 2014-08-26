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
        deriving (Show, Generic)

instance FromJSON Response
instance ToJSON Response

encode r = U.toStrict $ Data.Aeson.encode r :: SU.ByteString
decode r = Data.Aeson.decode (U.fromStrict r) :: Maybe Response

