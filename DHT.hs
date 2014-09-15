{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module DHT where

import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as U
import Peer
import Data.Maybe


type Key = T.Text

data Value = 
    ValPeer Peer | ValInt Int
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
addItemLine (Item hi i) (Line k Nothing) = 
    if T.take (T.length k) hi == k
    then Line k (Just (Item hi i))
    else Line k Nothing

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


genLine k = Line k Nothing
genEmptyDHT k = DHT k (map genLine (T.inits k))
