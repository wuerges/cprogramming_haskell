
{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Datatypes where

import Data.Aeson
import Data.Text
import GHC.Generics
import qualified Data.ByteString.Lazy.Char8 as U


data Peer = 
    Peer 
        { ip :: !Text
        , port :: Int
        , id :: !Text
        }
        deriving (Show, Generic)

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

data Request = 
    DownloadRequest
        { hash    :: !Text
        , number :: Int 
        } 
    | GetPeersResquest
        { sender   :: Peer
        } 
        deriving (Show, Generic)

instance FromJSON Peer
instance FromJSON Request
instance ToJSON Peer
instance ToJSON Request

