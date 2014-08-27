module Fileio where

import qualified Data.ByteString.Lazy as B
import qualified Crypto.Hash.SHA256 as C
import qualified Data.Map as Map
import System.Directory
import Control.Monad
import System.FilePath

newtype Hash = Hash B.ByteString
    deriving (Show, Ord, Eq)

data Part = Part { part :: B.ByteString
                 , h    :: Hash }
                 deriving Show

data File = File { hash :: Hash
                 , parts :: [Part] }
                 deriving Show

type FileMap  = Map.Map Hash File

hashes = Map.keys

getFilePart :: Hash -> Int -> FileMap -> Maybe Part
getFilePart h n fm = case Map.lookup h fm of
    Just f -> if n < length (parts f) then Just $ (parts f) !! n
                                      else Nothing
    Nothing -> Nothing


hashlazy bs = Hash $ B.fromStrict $ C.hashlazy bs

mkPart :: B.ByteString -> Part
mkPart bs = Part bs (hashlazy bs)

readParts :: FilePath -> IO File
readParts fp = do 
    f <- B.readFile fp
    return $ File (hashlazy f) (map mkPart $ splitEqual (1024 * 8) f)

splitEqual :: Int -> B.ByteString -> [B.ByteString]
splitEqual i bs = if B.length bs > i' 
                  then inits : splitEqual i ends
                  else [bs]
     where (inits, ends) = B.splitAt i' bs
           i' = fromIntegral i

loadFiles :: FilePath -> IO FileMap
loadFiles fp = do
    fps <- getDirectoryContents fp
    let filenames = [fp </> fi | fi <- fps]
    files <- filterM doesFileExist filenames
    files <- mapM readParts files
    return $ Map.fromList [(hash f, f) | f <- files]
