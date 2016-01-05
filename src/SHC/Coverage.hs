{-# LANGUAGE CPP               #-}
{-# LANGUAGE LambdaCase        #-}
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
import           Data.Aeson.Types           ()
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Digest.Pure.MD5
import           Data.Function
import           Data.List
import           Data.Maybe                 (fromMaybe)
import qualified Data.Map.Strict            as M
import           SHC.Lix
import           SHC.Types
import           SHC.Utils
import           System.Exit                (exitFailure)
import           System.FilePath            ((</>), normalise)
import           Trace.Hpc.Mix
import           Trace.Hpc.Tix
import           Trace.Hpc.Util

type ModuleCoverageData = ( BS.ByteString -- file source code
                          , Mix           -- module index data
                          , [Integer]     -- tixs recorded by HPC
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
            coverageData <- mapM getCoverageData tixs
            let filteredCoverageData = filter sourceDirFilter coverageData
            return $ M.fromList $ map toFirstAndRest filteredCoverageData
            where getCoverageData tixModule@(TixModule modName _ _ tixs) = do
                    let pkgKey = takeWhile (/= '/') modName
                        stackProj =
                          fromMaybe (error $ "readCoverageData/filePath/stackProj: couldn't find " ++ pkgKey) $
                          find ((== pkgKey) . stackProjectKey) (stackProjects conf)
                    dir <- SHC.Types.mixDir conf (stackProjectPath stackProj)
                    mix@(Mix origFp _ _ _ _) <- readMix [dir] (Right tixModule)
                    let fp = normalise $ maybe id (</>) (stackProjectPath stackProj) origFp
                    source <- BS.readFile fp
                    return (fp, source, mix, tixs)
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
           , "source_digest" .= (show . md5 . LBS.fromStrict) source
           , "coverage"      .= coverage
           ]
    where coverage = toSimpleCoverage converter lineCount mixEntriesTixs
          sourceLines = BS.lines source
          lineCount = length sourceLines
          mixEntriesTixs = groupMixEntryTixs mixEntryTixs
          mixEntryTixs = zip3 mixEntries tixs (map getExprSource' mixEntries)
          Mix _ _ _ _ mixEntries = mix
          getExprSource' = getExprSource $ map BS.unpack sourceLines

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
