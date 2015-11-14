module SHCSpec (spec)
    where

import Test.Hspec
import Test.Hspec.Contrib.HUnit
import Test.HUnit

import SHC.Types
import SHCHUnits


spec :: Spec
spec = do
    describe "SHC.Lix" $
        fromHUnitTest testToHit
    describe "SHC.Utils" $ do
        fromHUnitTest testMapFirst
        fromHUnitTest testMapLast
        fromHUnitTest testSubSeq
        fromHUnitTest testSubSubSeq
        fromHUnitTest testGroupByIndex
