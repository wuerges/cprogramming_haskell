{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Peer where

import Data.Aeson
import Data.Text
import GHC.Generics
import qualified Data.ByteString.Lazy.Char8 as U


data Peer = 
    Peer 
        { ip :: !Text
        , port :: Int
        , hash :: !Text
        }
        deriving (Show, Generic, Ord, Eq)

instance FromJSON Peer
instance ToJSON Peer

