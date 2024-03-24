{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main (main) where

import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO (IOMode (WriteMode), hPutStrLn, withFile)

main :: IO ()
main = do
  -- Uncomment this block to pass first stage
  let createParents = True
  createDirectoryIfMissing createParents ".git"
  createDirectoryIfMissing createParents (".git" </> "objects")
  createDirectoryIfMissing createParents (".git" </> "refs")
  writeFile (".git" </> "HEAD") ("ref: refs/heads/main" ++ "\n")
  putStrLn "Initialized git directory"
