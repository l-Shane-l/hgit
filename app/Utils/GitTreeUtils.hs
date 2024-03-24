module Utils.GitTreeUtils
  ( TreeEntry (..),
    sortEntriesByName,
    parseTreeObject,
    printEntry,
  )
where

import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.List (sortBy, unfoldr)
import Data.Ord (comparing)

-- Convert raw SHA to hexadecimal string representation
rawShaToHex :: B.ByteString -> String
rawShaToHex = BC.unpack . Base16.encode

data TreeEntry = TreeEntry
  { mode :: String,
    name :: String,
    sha :: String
  }
  deriving (Show, Eq)

parseTreeObject :: LB.ByteString -> Maybe [TreeEntry]
parseTreeObject content =
  let entriesBlob = LB.drop 1 $ LB.dropWhile (/= 0) content -- Drop the "tree <size>\0" header
   in Just $ parseEntries entriesBlob

parseEntries :: LB.ByteString -> [TreeEntry]
parseEntries blob
  | LB.null blob = []
  | otherwise =
      let (modeBytes, restAfterMode) = LB.span (/= 32) blob -- 32 is ASCII for space, separating mode
          mode = LC.unpack modeBytes
          (nameWithNullSha, restBlob) = LB.break (== 0) (LB.drop 1 restAfterMode) -- Skip space, find name
          name = LC.unpack nameWithNullSha
          shaBytes = LB.toStrict $ LB.take 20 $ LB.drop 1 restBlob -- Skip null byte for name, take SHA
          sha = rawShaToHex shaBytes
       in TreeEntry mode name sha : parseEntries (LB.drop 20 $ LB.drop 1 restBlob) -- Skip SHA, move to next entry

-- Print a single tree entry
printEntry :: TreeEntry -> IO ()
printEntry entry = putStrLn $ formatEntry entry

-- Format a tree entry as a string
formatEntry :: TreeEntry -> String
formatEntry (TreeEntry mode name sha) =
  mode ++ " " ++ determineType mode ++ " " ++ sha ++ "\t" ++ name

-- Helper function to determine entry type based on mode
determineType :: String -> String
determineType mode
  | mode == "040000" = "tree"
  | otherwise = "blob"

sortEntriesByName :: [TreeEntry] -> [TreeEntry]
sortEntriesByName = sortBy (comparing name)
