{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Request where

import Data.Aeson
import Data.Text
import GHC.Generics
import qualified Data.ByteString.Lazy.Char8 as U
import qualified Data.ByteString.Char8 as SU
import Peer

data Request = 
    DownloadRequest
        { hash    :: !Text
        , part_number :: Int 
        } 
    | GetPeersRequest
        { sender   :: Peer
        } 
        deriving (Show, Generic)

encode r = U.toStrict $ Data.Aeson.encode r :: SU.ByteString
decode r = Data.Aeson.decode (U.fromStrict r) :: Maybe Request

instance FromJSON Request
instance ToJSON Request

