module CatFile
  ( readGitBlobObject,
    outputContent,
  )
where

import Codec.Compression.Zlib (decompress)
import Control.DeepSeq (deepseq)
import Control.Exception (IOException, catch)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Char8 as LC
import System.FilePath ((</>))
import System.IO (IOMode (..), hGetContents, withBinaryFile)

readGitBlobObject :: String -> IO (Maybe LB.ByteString)
readGitBlobObject hash = do
  let path = gitObjectPath hash
  -- Try to read and decompress the git object. If it fails (e.g., file not found), catch the error and return Nothing.
  result <- catch (Just <$> readAndDecompressGitObject path) handleReadError
  case result of
    Just objectData -> return $ parseGitObject objectData
    Nothing -> return Nothing
  where
    handleReadError :: IOException -> IO (Maybe LB.ByteString)
    handleReadError _ = return Nothing

-- Constructs the file path to the Git object based on its hash
gitObjectPath :: String -> FilePath
gitObjectPath hash = ".git" </> "objects" </> take 2 hash </> drop 2 hash

-- Reads and decompresses a Git object from a given file path
readAndDecompressGitObject :: FilePath -> IO LB.ByteString
readAndDecompressGitObject path = withBinaryFile path ReadMode $ \handle -> do
  compressedData <- LB.hGetContents handle
  compressedData `deepseq` return $ decompress compressedData

-- Parses the decompressed Git object, ensuring it's a blob and extracting conten
parseGitObject :: LB.ByteString -> Maybe LB.ByteString
parseGitObject objectData =
  let -- Convert to a list of lazy ByteStrings separated by the null byte
      parts = LB.split 0 objectData
      -- Check if there are at least two parts (header and content) and the header starts with "blob"
      isValidBlob = length parts > 1 && LC.pack "blob " `LC.isPrefixOf` head parts
   in if isValidBlob
        then -- The actual content is after the first null byte, which is the second part of the list
          Just (parts !! 1)
        else Nothing

-- Outputs the given ByteString content to the console
outputContent :: LB.ByteString -> IO ()
outputContent = LB.putStr
