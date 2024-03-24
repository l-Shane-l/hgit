{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main (main) where

import CatFile
import HashObject
import Init
import System.Environment (getArgs)
import System.IO (IOMode (..), hGetContents, hPutStrLn, withBinaryFile, withFile)
import Utils

-- Parses command line arguments and executes the corresponding action
processArgs :: [String] -> IO ()
processArgs ("hash-object" : "-w" : filePath : _) = hashAndWriteObject filePath
processArgs ("cat-file" : "-p" : hash : _) = do
  content <- readGitBlobObject hash
  maybe (putStrLn "Error: Object not found or not a blob") outputContent content
processArgs ["init"] = initializeRepo
processArgs _ = putStrLn "Usage: <command> <args>"

main :: IO ()
main = do
  args <- getArgs
  processArgs args
