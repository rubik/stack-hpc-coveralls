module SHCSpec (spec)
    where

import Test.Hspec
import Test.Hspec.Contrib.HUnit
import Test.HUnit

import SHC.Types
import SHC.Lix


spec :: Spec
spec = do
    fromHUnitTest ("SHC.Lix" ~: "toHit" ~:
        [ Irrelevant @=? toHit []
        , None       @=? toHit [False]
        , None       @=? toHit [False, False]
        , Partial    @=? toHit [False, True]
        , Partial    @=? toHit [True, False]
        , Partial    @=? toHit [False, False, True]
        , Partial    @=? toHit [False, True, False]
        , Partial    @=? toHit [True, False, False]
        , Full       @=? toHit [True]
        , Full       @=? toHit [True, True]
        ])
