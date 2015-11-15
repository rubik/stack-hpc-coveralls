{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module SHCHUnits
    where

import Test.HUnit
import Trace.Hpc.Mix
import Trace.Hpc.Util

import SHC.Lix
import SHC.Types
import SHC.Utils


(~::) :: String -> [Assertion] -> Test
label ~:: tests = TestList $
    zipWith (\i -> TestLabel $ label ++ " " ++ show i) [1..] (map TestCase tests)

boxes :: [MixEntry]
boxes = [ (undefined, ExpBox False)
        , (undefined, BinBox GuardBinBox True)
        , (undefined, BinBox GuardBinBox False)
        ]

boxes2 :: [MixEntry]
boxes2 = map (\(_, b) -> (toHpcPos (1, 2, 3, 4), b)) boxes

testToHit :: Test
testToHit = "toHit" ~::
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
    ]

testToLineHit :: Test
testToLineHit = "toLineHit" ~::
    [ toLineHit ( [(toHpcPos (4, 5, 6, 2), undefined)]
                , [], undefined)     @?= (3, True)
    , toLineHit ( [(toHpcPos (4, 5, 6, 2), undefined)]
                , [1], undefined)    @?= (3, True)
    , toLineHit ( [(toHpcPos (4, 5, 6, 2), undefined)]
                , [0], undefined)    @?= (3, False)
    , toLineHit ( [(toHpcPos (4, 5, 6, 2), undefined)]
                , [0, 1], undefined) @?= (3, False)
    ]

testIsOtherwiseEntry :: Test
testIsOtherwiseEntry = "testIsOtherwiseEntry" ~::
    [ isOtherwiseEntry ([], undefined, [])               @?= False
    , isOtherwiseEntry ([], undefined, ["otherwise"])    @?= False
    , isOtherwiseEntry (boxes, undefined, [])            @?= False
    , isOtherwiseEntry (boxes, undefined, ["otherwise"]) @?= True
    ]

testAdjust :: Test
testAdjust = "testAdjust" ~::
    [ adjust (boxes2, [], ["otherwise"])  @?= (boxes2, [], ["otherwise"])
    , adjust (boxes2, [0], ["otherwise"]) @?= (boxes2, [0], ["otherwise"])
    , adjust (boxes2, [1], ["otherwise"]) @?= (boxes2, [1, 1, 1], ["otherwise"])
    , adjust ([], [0], ["otherwise"])     @?= ([], [0], ["otherwise"])
    , adjust ([], [0], [])                @?= ([], [0], [])
    ]



testMcons :: Test
testMcons = "mcons" ~::
    [ mcons Nothing []   @?= ([]::[Int])
    , mcons (Just 2) []  @?= [2]
    , mcons Nothing [4]  @?= [4]
    , mcons (Just 4) [2] @?= [4, 2]
    ]

testMatchAny :: Test
testMatchAny = "matchAny" ~::
    [ matchAny [] ""                   @?= False
    , matchAny [] "test"               @?= False
    , matchAny ["nothing"] "something" @?= False
    , matchAny ["a"] "also"            @?= True
    , matchAny ["a", "b"] "beta"       @?= True
    , matchAny ["a", "c"] "gamma"      @?= False
    ]

testMapFirst :: Test
testMapFirst = "mapFirst" ~::
    [ mapFirst (+ 1) []        @?= []
    , mapFirst (+ 1) [2]       @?= [3]
    , mapFirst (+ 1) [2, 3]    @?= [3, 3]
    , mapFirst (+ 1) [2, 3, 5] @?= [3, 3, 5]
    ]

testMapLast :: Test
testMapLast = "mapLast" ~::
    [ mapLast (+ 1) []        @?= []
    , mapLast (+ 1) [2]       @?= [3]
    , mapLast (+ 1) [2, 3]    @?= [2, 4]
    , mapLast (+ 1) [2, 3, 5] @?= [2, 3, 6]
    ]

testSubSeq :: Test
testSubSeq = "subSeq" ~::
    [ subSeq 0 0 []        @?= ([] :: [Int])
    , subSeq 0 0 [2]       @?= []
    , subSeq 0 1 [2]       @?= [2]
    , subSeq 0 2 [2]       @?= [2]
    , subSeq 1 1 [2]       @?= []
    , subSeq 1 2 [2]       @?= []
    , subSeq 0 0 [2, 3]    @?= []
    , subSeq 0 1 [2, 3]    @?= [2]
    , subSeq 0 2 [2, 3]    @?= [2, 3]
    , subSeq 0 3 [2, 3]    @?= [2, 3]
    , subSeq 1 1 [2, 3]    @?= []
    , subSeq 1 2 [2, 3]    @?= [3]
    , subSeq 1 3 [2, 3]    @?= [3]
    , subSeq 0 2 [2, 3]    @?= [2, 3]
    , subSeq 1 3 [2, 3, 5] @?= [3, 5]
    , subSeq 0 3 [2, 3, 5] @?= [2, 3, 5]
    ]

testSubSubSeq :: Test
testSubSubSeq = "subSubSeq" ~::
    [ subSubSeq 0 0 [[]]             @?= ([[]] :: [[Int]])
    , subSubSeq 0 0 [[2]]            @?= [[]]
    , subSubSeq 0 1 [[2]]            @?= [[2]]
    , subSubSeq 0 0 [[2, 3]]         @?= [[]]
    , subSubSeq 0 1 [[2, 3]]         @?= [[2]]
    , subSubSeq 0 2 [[2, 3]]         @?= [[2, 3]]
    , subSubSeq 1 1 [[2, 3]]         @?= [[]]
    , subSubSeq 1 2 [[2, 3]]         @?= [[3]]
    , subSubSeq 1 3 [[2, 3]]         @?= [[3]]
    , subSubSeq 0 2 [[2, 3]]         @?= [[2, 3]]
    , subSubSeq 1 3 [[2, 3, 5]]      @?= [[3, 5]]
    , subSubSeq 0 3 [[2, 3, 5]]      @?= [[2, 3, 5]]
    , subSubSeq 0 0 [[2, 3], [5, 7]] @?= [[2, 3], []]
    , subSubSeq 0 1 [[2, 3], [5, 7]] @?= [[2, 3], [5]]
    , subSubSeq 0 2 [[2, 3], [5, 7]] @?= [[2, 3], [5, 7]]
    , subSubSeq 1 0 [[2, 3], [5, 7]] @?= [[3], []]
    , subSubSeq 1 1 [[2, 3], [5, 7]] @?= [[3], [5]]
    , subSubSeq 1 2 [[2, 3], [5, 7]] @?= [[3], [5, 7]]
    , subSubSeq 2 0 [[2, 3], [5, 7]] @?= [[], []]
    , subSubSeq 2 1 [[2, 3], [5, 7]] @?= [[], [5]]
    , subSubSeq 2 2 [[2, 3], [5, 7]] @?= [[], [5, 7]]
    ]

testGroupByIndex :: Test
testGroupByIndex = "groupByIndex" ~::
    [ groupByIndex 0 [(0, 2)]         @?= []
    , groupByIndex 1 [(0, 2)]         @?= [[2]]
    , groupByIndex 2 [(0, 2)]         @?= [[2], []]
    , groupByIndex 0 [(1, 2)]         @?= []
    , groupByIndex 1 [(1, 2)]         @?= [[]]
    , groupByIndex 2 [(1, 2)]         @?= [[], [2]]
    , groupByIndex 3 [(1, 2)]         @?= [[], [2], []]
    , groupByIndex 0 [(0, 2), (0, 3)] @?= []
    , groupByIndex 1 [(0, 2), (0, 3)] @?= [[3, 2]]
    , groupByIndex 1 [(0, 2), (1, 3)] @?= [[2]]
    , groupByIndex 1 [(1, 2), (1, 3)] @?= [[]]
    , groupByIndex 2 [(0, 2), (0, 3)] @?= [[3, 2], []]
    , groupByIndex 2 [(0, 2), (1, 3)] @?= [[2], [3]]
    , groupByIndex 2 [(1, 2), (1, 3)] @?= [[], [3, 2]]
    , groupByIndex 3 [(0, 2), (0, 3)] @?= [[3, 2], [], []]
    , groupByIndex 3 [(0, 2), (1, 3)] @?= [[2], [3], []]
    , groupByIndex 3 [(1, 2), (1, 3)] @?= [[], [3, 2], []]
    , groupByIndex 5 [(0, 2), (2, 5), (2, 3), (4, 13), (4, 11), (4, 7)] @?= [[2], [], [3, 5], [], [7, 11, 13]]
    ]
