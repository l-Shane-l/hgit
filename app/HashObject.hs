module HashObject
  ( hashAndWriteObject,
  )
where

import Codec.Compression.Zlib (compress)
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.Digest.Pure.SHA (sha1, showDigest)
import Utils.Index (readFileContents, writeGitObject)

hashAndWriteObject :: FilePath -> IO ()
hashAndWriteObject filePath = do
  content <- readFileContents filePath
  let header = LC.pack $ "blob " ++ show (LC.length content) ++ "\0"
      object = header `LC.append` content
      sha = showDigest $ sha1 object
      compressed = compress object
  writeGitObject sha compressed
  putStrLn sha
