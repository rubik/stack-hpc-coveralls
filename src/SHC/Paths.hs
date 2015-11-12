-- |
-- Module:      Trace.Hpc.Coveralls.Paths
-- Copyright:   (c) 2014-2015 Guillaume Nargeot
-- License:     BSD3
-- Maintainer:  Guillaume Nargeot <guillaume+hackage@nargeot.com>
-- Stability:   experimental
--
-- Paths constants and functions for hpc coverage report output.

module SHC.Paths
    where

import Control.Monad
import Data.Maybe
import Data.List (isSuffixOf)
import Data.Traversable (traverse)
import System.Directory (
    doesDirectoryExist, getDirectoryContents
    )
import System.Directory.Tree (
    AnchoredDirTree(..), dirTree, readDirectoryWith
    )
import System.FilePath ((</>))
import Trace.Hpc.Tix


tixDir :: FilePath -> FilePath
tixDir = (</> "tix")

mixDir :: FilePath -> FilePath
mixDir = (</> "mix")

getTixPath :: FilePath -> String -> FilePath
getTixPath hpcDir testSuiteName = tixDir hpcDir </> testSuiteName </> getTixFileName testSuiteName

firstExistingDirectory :: [FilePath] -> IO (Maybe FilePath)
firstExistingDirectory = fmap msum . mapM pathIfExist
    where pathIfExist path = do
              pathExists <- doesDirectoryExist path
              return $ if pathExists then Just path else Nothing

dumpDirectory :: FilePath -> IO ()
dumpDirectory path = do
    directoryExists <- doesDirectoryExist path
    unless directoryExists $ putStrLn ("Couldn't find the directory " ++ path)
    putStrLn $ "Dumping " ++ path ++ " directory content:"
    contents <- getDirectoryContents path
    traverse putStrLn contents
    return ()

dumpDirectoryTree :: FilePath -> IO ()
dumpDirectoryTree path = do
    putStrLn $ "Dumping " ++ path ++ " directory tree:"
    tree <- readDirectoryWith return path
    traverse putStrLn $ dirTree tree
    return ()
