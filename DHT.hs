{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module DHT where

import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as U
import Peer
import Data.Maybe


newtype Key = Key T.Text
    deriving (Show, Ord, Eq)

data Value = 
    ValPeer Peer | ValInt Int | ValHash !T.Text
    deriving Show

data Item = Item Key Value
    deriving Show


{- 
 - A line contains a prefix and an item that matches to that prefix.
 -}
data Line = Line Key (Maybe Item)
    deriving Show

{- Adds an item to a line -}
addItemLine :: Item -> Line -> Line
addItemLine (Item (Key hi) i) (Line (Key k) Nothing) = 
    if T.take (T.length k) hi == k
    then Line (Key k) (Just (Item (Key hi) i))
    else Line (Key k) Nothing

getItemLine k (Line _ i) = case i of
    Just (Item hi ii) -> if k == hi 
                         then Just (Item hi ii)
                         else Nothing
    Nothing ->  Nothing


{- 
 - A DHT has a key, and a list of lines, each line with a prefix 
 - that is increasingly similar to the key
 -}
data DHT = DHT Key [Line]
    deriving Show

{-
 - Adds an item to the DHT
 -}
addItemDHT i (DHT k ls) = DHT k (map (addItemLine i) ls)

locateItemDHT k (DHT _ ls) = catMaybes (map (getItemLine k) ls)


simplifyHash :: T.Text -> Key
simplifyHash t = Key t

genLine k = Line (Key k) Nothing
genEmptyDHT (Key k) = DHT (Key k) (map genLine (T.inits k))
