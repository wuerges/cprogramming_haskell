{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Hex where

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import Data.Hex as H
import Data.Maybe

--fromMonad :: Monad h -> h
--fromMonad (Monad h) = h

unhex :: T.Text -> B.ByteString
unhex t = fromJust $ H.unhex $ encodeUTF t 

hex :: B.ByteString -> T.Text
hex b = decodeUTF $ H.hex b

lhex :: S.ByteString -> T.Text
lhex b = E.decodeUtf8 $ H.hex b

decodeUTF :: B.ByteString -> T.Text
decodeUTF = E.decodeUtf8 . B.toStrict

encodeUTF :: T.Text -> B.ByteString
encodeUTF = B.fromStrict . E.encodeUtf8

{-
unhex1 :: T.Text -> Word8
unhex1 t = read t

unhex :: T.Text -> U.ByteString
unhex t = map unhex1 pairs
    where pairs = splitPairs t

hex :: U.ByteString -> T.Text
hex b = ""


splitPairs :: T.Text -> [T.Text]
splitPairs bs = if T.length bs <= 2
                  then [bs]
                  else inits : splitPairs ends
     where (inits, ends) = T.splitAt 2 bs

-}
