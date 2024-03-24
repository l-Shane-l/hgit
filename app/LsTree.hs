module LsTree
  ( lsTree,
  )
where

import Utils.Index (TreeEntry (..), gitObjectPath, parseTreeObject, printEntry, readAndDecompressGitObject, sortEntriesByName)

-- Function to handle the 'ls-tree' command
lsTree :: String -> Bool -> IO ()
lsTree treeSha nameOnly = do
  let path = gitObjectPath treeSha
  content <- readAndDecompressGitObject path
  case parseTreeObject content of
    Just entries -> do
      let sortedEntries = sortEntriesByName entries
      if nameOnly
        then mapM_ (putStrLn . name) sortedEntries
        else mapM_ printEntry sortedEntries
    Nothing -> putStrLn "Error: Invalid tree object"
