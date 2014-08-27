{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import Test.HUnit
import Request
import Response
import Peer
import Data.Maybe

testRequestCoding r = TestCase (assertEqual "test_request_coding" (fromMaybe null_request $ Request.decode $ Request.encode r) r)
testResponseCoding r = TestCase (assertEqual "test_response_coding" (fromMaybe null_response $ Response.decode $ Response.encode r) r)

peer1 = Peer "127.0.0.1" 5005 "hash_request1"

request1 = DownloadRequest "hash_request1" 0
null_request = DownloadRequest "null" 0
request2 = GetPeersRequest peer1


some_data1 = "very very long data"

null_response = GetPeersResponse []
response1 = GetPeersResponse [peer1]
response2 = DownloadResponse "hash_part1" (T.length some_data1) 0 some_data1


test_request_encodings = TestList $ map testRequestCoding [request1, request2]
test_response_encodings = TestList $ map testRequestCoding [request1, request2]

testlist = TestList [test_request_encodings, test_response_encodings]
