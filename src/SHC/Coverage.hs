{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module:      SHC.Coverage
-- Copyright:   (c) 2015 Michele Lacchia
-- License:     ISC
-- Maintainer:  Michele Lacchia <michelelacchia@gmail.com>
-- Stability:   experimental
--
-- Functions for reading, converting and sending HPC output to coveralls.io.

module SHC.Coverage
    where

#if __GLASGOW_HASKELL__ < 710
import           Control.Applicative
#endif
import           Data.Aeson
import           Data.Aeson.Types ()
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Digest.Pure.MD5
import           Data.Function
import           Data.List
import qualified Data.Map.Strict as M
import           System.FilePath ((</>))
import           System.Exit (exitFailure)
import           SHC.Types
import           SHC.Utils
import           SHC.Lix
import           Trace.Hpc.Mix
import           Trace.Hpc.Tix
import           Trace.Hpc.Util

type ModuleCoverageData = ( String     -- file source code
                          , Mix        -- module index data
                          , [Integer]  -- tixs recorded by HPC
                          )

type TestSuiteCoverageData = M.Map FilePath ModuleCoverageData

-- Is there a way to restrict this to only Number and Null?
type CoverageValue = Value

-- Single file coverage data in the format defined by coveralls.io
type SimpleCoverage = [CoverageValue]
type LixConverter = Lix -> SimpleCoverage

strictConverter :: LixConverter
strictConverter = map $ \case
    Full       -> Number 1
    Partial    -> Number 0
    None       -> Number 0
    Irrelevant -> Null

looseConverter :: LixConverter
looseConverter = map $ \case
    Full       -> Number 2
    Partial    -> Number 1
    None       -> Number 0
    Irrelevant -> Null

readMix' :: Config -> TixModule -> IO Mix
readMix' conf tix = readMix [SHC.Types.mixDir conf] (Right tix)

-- | Generate Coveralls JSON formatted code coverage from HPC coverage data
generateCoverallsFromTix :: Config -> IO Value
generateCoverallsFromTix conf = do
    testSuitesCoverages <- mapM (readCoverageData conf) testSuitesName
    let coverageData = mergeCoverageData testSuitesCoverages
    return $ toCoverallsJson conf converter coverageData
    where testSuitesName = suitesName conf
          converter = case conversion conf of
              FullLines -> strictConverter
              PartialLines -> looseConverter

-- | Create a list of coverage data from the tix input
readCoverageData :: Config -> String -> IO TestSuiteCoverageData
readCoverageData conf suite = do
    let tixPath = hpcDir conf </> suite </> getTixFileName suite
    mTix <- readTix tixPath
    case mTix of
        Nothing -> putStrLn ("Couldn't find the file " ++ tixPath) >>
                   exitFailure
        Just (Tix tixs) -> do
            mixs <- mapM (readMix' conf) tixs
            let files = map filePath mixs
            sources <- mapM readFile files
            let coverageData = zip4 files sources mixs (map tixModuleTixs tixs)
            let filteredCoverageData = filter sourceDirFilter coverageData
            return $ M.fromList $ map toFirstAndRest filteredCoverageData
            where filePath (Mix fp _ _ _ _) = fp
                  sourceDirFilter = not . matchAny excludeDirPatterns . fst4
                  excludeDirPatterns = []  -- XXX: for now

toCoverallsJson :: Config -> LixConverter -> TestSuiteCoverageData -> Value
toCoverallsJson conf converter testSuiteCoverageData =
    object $ if serviceName conf == "travis-ci"
                then withRepoToken
                else withGitInfo
    where base = [ "service_job_id" .= jobId conf
                 , "service_name"   .= serviceName conf
                 , "source_files"   .= toJsonList testSuiteCoverageData
                 ]
          toJsonList = map (uncurry $ coverageToJson converter) . M.toList
          withRepoToken = mcons (("repo_token" .=) <$> repoToken conf) base
          withGitInfo   = ("git" .= gitInfo conf) : withRepoToken

coverageToJson :: LixConverter -> FilePath -> ModuleCoverageData -> Value
coverageToJson converter path (source, mix, tixs) =
    object [ "name"          .= path
           , "source_digest" .= (show . md5 . LBS.pack) source
           , "coverage"      .= coverage
           ]
    where coverage = toSimpleCoverage converter lineCount mixEntriesTixs
          lineCount = length $ lines source
          mixEntriesTixs = groupMixEntryTixs mixEntryTixs
          mixEntryTixs = zip3 mixEntries tixs (map getExprSource' mixEntries)
          Mix _ _ _ _ mixEntries = mix
          getExprSource' = getExprSource $ lines source

mergeCoverageData :: [TestSuiteCoverageData] -> TestSuiteCoverageData
mergeCoverageData = foldr1 $ M.unionWith mergeModule
    where mergeModule (source, mix, tixs1) (_, _, tixs2) =
            (source, mix, zipWith (+) tixs1 tixs2)

toSimpleCoverage :: LixConverter -> Int -> [CoverageEntry] -> SimpleCoverage
toSimpleCoverage convert lineCount = convert . toLix lineCount

getExprSource :: [String] -> MixEntry -> [String]
getExprSource source (hpcPos, _) = subSubSeq startCol endCol subLines
    where subLines  = subSeq startLine endLine source
          startLine = startLine' - 1
          startCol  = startCol'  - 1
          (startLine', startCol', endLine, endCol) = fromHpcPos hpcPos

groupMixEntryTixs :: [(MixEntry, Integer, [String])] -> [CoverageEntry]
groupMixEntryTixs = map mergeOnLst3 . groupBy ((==) `on` fst . fst3)
    where mergeOnLst3 xxs@(x:_) = (map fst3 xxs, map snd3 xxs, trd3 x)
          mergeOnLst3 [] = error "mergeOnLst3 applied to empty list"
