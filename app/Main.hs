{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main (main) where

import CatFile
import Codec.Compression.Zlib (decompress)
import Control.Exception (IOException, catch)
import Control.Monad (when)
import Crypto.Cipher.ChaCha (initialize)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Char8 as LC
import Init
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitWith)
import System.FilePath ((</>))
import System.IO (IOMode (..), hGetContents, hPutStrLn, withBinaryFile, withFile)

-- Parses command line arguments and executes the corresponding action
processArgs :: [String] -> IO ()
processArgs ("cat-file" : "-p" : hash : _) = do
  content <- readGitBlobObject hash
  maybe (putStrLn "Error: Object not found or not a blob") outputContent content
processArgs ["init"] = initializeRepo
processArgs _ = putStrLn "Usage: <command> <args>"

main :: IO ()
main = do
  args <- getArgs
  processArgs args
