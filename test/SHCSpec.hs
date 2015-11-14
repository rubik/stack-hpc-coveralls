{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module SHCSpec (spec)
    where

import Prelude hiding (getLine)
import Test.Hspec
import Test.Hspec.Contrib.HUnit
import Trace.Hpc.Util
import Control.DeepSeq (force)
import Control.Exception (evaluate)

import SHCHUnits

import SHC.Lix
import SHC.Utils


spec :: Spec
spec = do
    describe "SHC.Lix" $ do
        describe "toHit" $
            fromHUnitTest testToHit
        it "getLine" $
            getLine (toHpcPos (1, 2, 3, 4), undefined) `shouldBe` 1
        describe "toLineHit" $ do
            it "throws an exception with empty lists" $
                evaluate (force (toLineHit ([], [], [""]))) `shouldThrow` anyException
            fromHUnitTest testToLineHit
        describe "isOtherwiseEntry" $
            fromHUnitTest testIsOtherwiseEntry
        describe "adjust" $
            fromHUnitTest testAdjust
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
