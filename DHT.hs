{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module DHT where

import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as U
import Peer
import Data.Maybe
import Data.Char
import Data.List


newtype Key = Key [Int]
    deriving (Show, Ord, Eq)

data Value a = Value a
    deriving Show

data Item a = Item Key (Value a)
    deriving Show

mkItemDHT :: T.Text -> a -> Item a
mkItemDHT t p = Item (simplifyHash t) (Value p)

{- 
 - A line contains a prefix and an item that matches to that prefix.
 -}
data Line a = Line Key (Maybe (Item a))
    deriving Show

{- Adds an item to a line -}
addItemLine :: Item a -> Line a -> Line a
addItemLine (Item (Key hi) i) (Line (Key k) Nothing) = 
    if take (length k) hi == k
    then Line (Key k) (Just (Item (Key hi) i))
    else Line (Key k) Nothing

addItemLine i (Line k (Just v)) = Line k (Just v)


getItemLine k (Line _ i) = case i of
    Just (Item hi ii) -> if k == hi 
                         then Just (Item hi ii)
                         else Nothing
    Nothing ->  Nothing


{- 
 - A DHT has a key, and a list of lines, each line with a prefix 
 - that is increasingly similar to the key
 -}
data DHT a = DHT Key [Line a]
    deriving Show

{-
 - Adds an item to the DHT
 -}
addItemDHT i (DHT k ls) = DHT k (map (addItemLine i) ls)

locateItemDHT k (DHT _ ls) = catMaybes (map (getItemLine k) ls)


mod4 n = n `mod` 4

simplifyHash :: T.Text -> Key
simplifyHash t = Key (map (mod4 . ord) $ T.unpack t)

genLine k = Line (Key k) Nothing
genEmptyDHT (Key k) = DHT (Key k) (map genLine (inits k))
