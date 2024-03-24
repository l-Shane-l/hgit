module Init (initializeRepo) where

import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

initializeRepo :: IO ()
initializeRepo = do
  let createParents = True
  createDirectoryIfMissing createParents ".git"
  createDirectoryIfMissing createParents (".git" </> "objects")
  createDirectoryIfMissing createParents (".git" </> "refs")
  writeFile (".git" </> "HEAD") "ref: refs/heads/main\n"
  putStrLn "Initialized git directory"
