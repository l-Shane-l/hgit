module Utils.GitObjectUtils
  ( readAndDecompressGitObject,
    writeGitObject,
    gitObjectPath,
    readFileContents,
    outputContent,
  )
where

import Codec.Compression.Zlib (decompress)
import Control.DeepSeq (deepseq)
import qualified Data.ByteString.Lazy as LB
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO (IOMode (..), hGetContents, withBinaryFile)

-- Reads and decompresses a Git object from a given file path
readAndDecompressGitObject :: FilePath -> IO LB.ByteString
readAndDecompressGitObject path = withBinaryFile path ReadMode $ \handle -> do
  compressedData <- LB.hGetContents handle
  compressedData `deepseq` return $ decompress compressedData

writeGitObject :: String -> LB.ByteString -> IO ()
writeGitObject sha compressed = do
  let (dir, file) = splitAt 2 sha
      path = ".git" </> "objects" </> dir
  createDirectoryIfMissing True path
  LB.writeFile (path </> file) compressed

gitObjectPath :: String -> FilePath
gitObjectPath hash = ".git" </> "objects" </> take 2 hash </> drop 2 hash

readFileContents :: FilePath -> IO LB.ByteString
readFileContents = LB.readFile

-- Outputs the given ByteString content to the console
outputContent :: LB.ByteString -> IO ()
outputContent = LB.putStr
