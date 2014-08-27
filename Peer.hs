{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Peer where

import Data.Aeson
import Data.Maybe
import qualified Data.Text as T
import GHC.Generics
import qualified Data.ByteString.Lazy.Char8 as U


data Peer = 
    Peer 
        { ip :: !T.Text
        , port :: Int
        , hash :: !T.Text
        }
        deriving (Show, Generic, Ord, Eq)


ipS :: Peer -> String
ipS p = T.unpack $ ip p

portS :: Peer -> String
portS p = show $ port p

instance FromJSON Peer
instance ToJSON Peer


decodePeersS = decodePeers . U.pack

decodePeers :: U.ByteString -> [Peer]
decodePeers x = fromMaybe [] (decode x)
