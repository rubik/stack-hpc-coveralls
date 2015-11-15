{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE OverloadedStrings #-}
module SHCSpec (spec)
    where

import Data.Aeson
import Data.Time.Clock (getCurrentTime)
import qualified Data.Map.Strict as M
import Test.Hspec
import Test.Hspec.Contrib.HUnit
import Trace.Hpc.Mix
import Trace.Hpc.Util
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import System.IO.Unsafe (unsafePerformIO)

import SHCHUnits

import SHC.Api
import SHC.Lix
import SHC.Types
import SHC.Utils
import SHC.Coverage


covEntries :: [CoverageEntry]
covEntries =
    [ ([(toHpcPos (1, 2, 3, 4), ExpBox True)], [1, 2, 3, 0, 1, 4], ["a"])
    , ([(toHpcPos (1, 2, 3, 8), ExpBox True)], [1, 2, 3, 3, 1, 4], ["a"])
    , ([(toHpcPos (2, 5, 6, 7), ExpBox True)], [1, 2, 3, 4, 2, 4], ["b"])
    , ([(toHpcPos (4, 8, 9, 9), ExpBox True)], [0], ["c"])
    ]

mix :: Mix
mix = Mix undefined undefined undefined undefined boxes2

mix2 :: Mix
mix2 = Mix "path" (unsafePerformIO getCurrentTime) (toHash 'c') 3 boxes2

spec :: Spec
spec = do
    describe "SHC.Coverage" $ do
        describe "toSimpleCoverage" $ do
            it "works with strictConverter" $
                toSimpleCoverage strictConverter 4 covEntries `shouldBe`
                    [Number 0, Number 1, Null, Number 0]
            it "works with looseConverter" $
                toSimpleCoverage looseConverter 4 covEntries `shouldBe`
                    [Number 1, Number 2, Null, Number 0]
        describe "coverageToJson" $
            it "works with standard coverage data" $
                coverageToJson strictConverter "path"
                               ("a\nb\nc\n", mix, [1, 2, 3])
                    `shouldBe`
                    object [ "name"          .= ("path"::String)
                           , "source_digest" .= ("40c53c58fdafacc83cfff6ee3d2f6d69"::String)
                           , "coverage"      .= [Number 1, Null, Null]
                           ]
        describe "mergeCoverageData" $
            it "works with standard coverage data" $
                mergeCoverageData
                     [ M.fromList [ ("path1", ("", mix2, [1, 2, 3]))
                                  , ("path2", ("a", mix2, [4, 5, 6]))
                                  ]
                     , M.fromList [("path1", ("c", mix2, [8, 1, 3]))]
                     ]
                    `shouldBe` M.fromList [ ("path1", ("", mix2, [9, 3, 6]))
                                          , ("path2", ("a", mix2, [4, 5, 6]))
                                          ]
    describe "SHC.Lix" $ do
        describe "toHit" $
            fromHUnitTest testToHit
        it "startLine" $
            startLine (toHpcPos (1, 2, 3, 4), undefined) `shouldBe` 1
        describe "toLineHit" $ do
            it "throws an exception with empty lists" $
                evaluate (force (toLineHit ([], [], [""]))) `shouldThrow` anyException
            fromHUnitTest testToLineHit
        describe "isOtherwiseEntry" $
            fromHUnitTest testIsOtherwiseEntry
        describe "adjust" $
            fromHUnitTest testAdjust
        it "toLix" $
            toLix 4 covEntries `shouldBe` [Partial, Full, Irrelevant, None]
    describe "SHC.Utils" $ do
        it "fst3" $ fst3 (1, 2, 3) `shouldBe` 1
        it "snd3" $ snd3 (1, 2, 3) `shouldBe` 2
        it "trd3" $ trd3 (1, 2, 3) `shouldBe` 3
        it "fst4" $ fst4 (1, 2, 3, 4) `shouldBe` 1
        it "toFirstAndRest" $
            toFirstAndRest (1, 2, 3, 4) `shouldBe` (1, (2, 3, 4))
        describe "mcons" $
            fromHUnitTest testMcons
        describe "matchAny" $
            fromHUnitTest testMatchAny
        describe "mapFirst" $
            fromHUnitTest testMapFirst
        describe "mapLast" $
            fromHUnitTest testMapLast
        describe "subSeq" $
            fromHUnitTest testSubSeq
        describe "subSubSeq" $
            fromHUnitTest testSubSubSeq
        describe "groupByIndex" $
            fromHUnitTest testGroupByIndex
