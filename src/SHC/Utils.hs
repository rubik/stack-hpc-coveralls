{-# LANGUAGE CPP #-}
-- |
-- Module:      SHC.Utils
-- Copyright:   (c) 2014-2015 Guillaume Nargeot
-- License:     BSD3
-- Maintainer:  Michele Lacchia <michelelacchia@gmail.com>
-- Stability:   experimental
--
-- Utility functions used in other modules.

module SHC.Utils
    where

import           Control.Monad       (guard)
import           Data.Function       (on)
import           Data.List
import           Data.Version
#if __GLASGOW_HASKELL__ < 710
import           Control.Applicative ((<$>), (<*>))
#endif
import           Text.ParserCombinators.ReadP
import           System.FilePath     ((</>))
import           System.Process      (readProcess)

import           SHC.Types


readP :: String -> [String] -> IO String
readP cmd args = init <$> readProcess cmd args []  -- strip trailing \n

git :: [String] -> IO String
git = readP "git"

stack :: [String] -> IO String
stack = readP "stack"

-- | Get information about the Git repo in the current directory.
getGitInfo :: IO GitInfo
getGitInfo = GitInfo <$> headRef <*> branchName <*> getRemotes
    where headRef = Commit <$> git ["rev-parse", "HEAD"]
                           <*> git ["log", "-1", "--pretty=%aN"]
                           <*> git ["log", "-1", "--pretty=%aE"]
                           <*> git ["log", "-1", "--pretty=%cN"]
                           <*> git ["log", "-1", "--pretty=%cE"]
                           <*> git ["log", "-1", "--pretty=%s"]
          branchName = git ["rev-parse", "--abbrev-ref", "HEAD"]

getRemotes :: IO [Remote]
getRemotes = nubBy ((==) `on` name) <$> parseRemotes <$> git ["remote", "-v"]
    where parseRemotes :: String -> [Remote]
          parseRemotes input = do
            line <- lines input
            let fields = words line
            guard $ length fields >= 2
            return $ Remote (head fields) (fields !! 1)

-- | Verify that the required Stack is present.
checkStackVersion :: IO Bool
checkStackVersion = do
    let lowerBound = Version [0,1,7,0] []
    stackVersion <- stack ["--numeric-version"]
    return $ verifyVersion stackVersion lowerBound

-- | Check whether a string is a version and if it is
-- greater than or equal to a specified version.
verifyVersion :: String -> Version -> Bool
verifyVersion ver lowerBound =
        let parses = readP_to_S parseVersion ver
        in  not (null parses) && (lowerBound <= fst (last parses))

-- | Return the HPC data directory, given the package name.
getHpcDir :: String -> IO FilePath
getHpcDir package = (</> package) <$> stack ["path", "--local-hpc-root"]

-- | Return the HPC mix directory, where module data is stored.
getMixDir :: IO FilePath
getMixDir = (</> "hpc") <$> stack ["path", "--dist-dir"]

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

snd3 :: (a, b, c) -> b
snd3 (_, x, _) = x

trd3 :: (a, b, c) -> c
trd3 (_, _, x) = x

fst4 :: (a, b, c, d) -> a
fst4 (x, _, _, _) = x

toFirstAndRest :: (a, b, c, d) -> (a, (b, c, d))
toFirstAndRest (a, b, c, d) = (a, (b, c, d))

mcons :: Maybe a -> [a] -> [a]
mcons Nothing xs = xs
mcons (Just x) xs = x:xs

matchAny :: [String] -> String -> Bool
matchAny patterns fileName = any (`isPrefixOf` fileName) patterns

mapFirst :: (a -> a) -> [a] -> [a]
mapFirst f (x:xs) = f x : xs
mapFirst _ []     = []

mapLast :: (a -> a) -> [a] -> [a]
mapLast f [x]    = [f x]
mapLast f (x:xs) = x : mapLast f xs
mapLast _ []     = []

subSeq :: Int -> Int -> [a] -> [a]
subSeq start end = drop start . take end

subSubSeq :: Int -> Int -> [[a]] -> [[a]]
subSubSeq start end = mapFirst (drop start) . mapLast (take end)

groupByIndex :: Int -> [(Int, a)] -> [[a]]
groupByIndex size = take size . flip (++) (repeat []) . groupByIndex' 0 []
    where groupByIndex' _ ys [] = [ys]
          groupByIndex' i ys xx@((xi, x):xs) = if xi == i
              then groupByIndex' i (x:ys) xs
              else ys : groupByIndex' (i + 1) [] xx
