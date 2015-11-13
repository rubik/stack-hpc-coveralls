-- |
-- Module:      SHC.Lix
-- Copyright:   (c) 2014-2015 Guillaume Nargeot
-- License:     BSD3
-- Maintainer:  Michele Lacchia <michelelacchia@gmail.com>
-- Stability:   experimental
-- Portability: portable
--
-- Functions for converting HPC output to line-based code coverage data.

module SHC.Lix
    where

import Data.Ord
import Data.List
import Prelude hiding (getLine)
import SHC.Types
import SHC.Utils
import Trace.Hpc.Mix
import Trace.Hpc.Util

toHit :: [Bool] -> Hit
toHit []  = Irrelevant
toHit [x] = if x then Full else None
toHit xs
    | and xs    = Full
    | or xs     = Partial
    | otherwise = None

getLine :: MixEntry -> Int
getLine = fst4 . fromHpcPos . fst

toLineHit :: CoverageEntry -> (Int, Bool)
toLineHit (entries, counts, _) = (getLine (head entries) - 1, all (> 0) counts)

isOtherwiseEntry :: CoverageEntry -> Bool
isOtherwiseEntry (mixEntries, _, source) =
    source == ["otherwise"] && boxLabels == otherwiseBoxLabels
    where boxLabels = map snd mixEntries
          otherwiseBoxLabels = [ ExpBox False
                               , BinBox GuardBinBox True
                               , BinBox GuardBinBox False
                               ]

adjust :: CoverageEntry -> CoverageEntry
adjust coverageEntry@(mixEntries, tixs, source) =
    if isOtherwiseEntry coverageEntry && any (> 0) tixs
    then (mixEntries, [1, 1, 1], source)
    else coverageEntry

-- | Convert hpc coverage entries into a line based coverage format
toLix :: Int             -- ^ Source line count
      -> [CoverageEntry] -- ^ Mix entries and associated hit count
      -> Lix             -- ^ Line coverage
toLix lineCount entries = map toHit (groupByIndex lineCount sortedLineHits)
    where sortedLineHits = sortBy (comparing fst) lineHits
          lineHits = map (toLineHit . adjust) entries
