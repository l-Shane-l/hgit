module Utils
  ( readFileContents,
    writeGitObject,
    gitObjectPath,
    outputContent,
  )
where

import qualified Data.ByteString.Lazy as LB
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

readFileContents :: FilePath -> IO LB.ByteString
readFileContents = LB.readFile

writeGitObject :: String -> LB.ByteString -> IO ()
writeGitObject sha compressed = do
  let (dir, file) = splitAt 2 sha
      path = ".git" </> "objects" </> dir
  createDirectoryIfMissing True path
  LB.writeFile (path </> file) compressed

gitObjectPath :: String -> FilePath
gitObjectPath hash = ".git" </> "objects" </> take 2 hash </> drop 2 hash

-- Outputs the given ByteString content to the console
outputContent :: LB.ByteString -> IO ()
outputContent = LB.putStr
