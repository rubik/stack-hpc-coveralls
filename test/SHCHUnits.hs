module SHCHUnits
    where

import Test.Hspec
import Test.HUnit

import SHC.Lix
import SHC.Types
import SHC.Utils


(~::) :: String -> [Assertion] -> Test
label ~:: tests = TestList $
    zipWith (\i -> TestLabel $ label ++ " " ++ show i) [1..] (map TestCase tests)

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

testMapFirst = "mapFirst" ~:: [
    mapFirst (+ 1) [] @?= [],
    mapFirst (+ 1) [2] @?= [3],
    mapFirst (+ 1) [2, 3] @?= [3, 3],
    mapFirst (+ 1) [2, 3, 5] @?= [3, 3, 5]]

testMapLast = "mapLast" ~:: [
    mapLast (+ 1) [] @?= [],
    mapLast (+ 1) [2] @?= [3],
    mapLast (+ 1) [2, 3] @?= [2, 4],
    mapLast (+ 1) [2, 3, 5] @?= [2, 3, 6]]

testSubSeq = "subSeq" ~:: [
    subSeq 0 0 [] @?= ([] :: [Int]),
    subSeq 0 0 [2] @?= [],
    subSeq 0 1 [2] @?= [2],
    subSeq 0 2 [2] @?= [2],
    subSeq 1 1 [2] @?= [],
    subSeq 1 2 [2] @?= [],
    subSeq 0 0 [2, 3] @?= [],
    subSeq 0 1 [2, 3] @?= [2],
    subSeq 0 2 [2, 3] @?= [2, 3],
    subSeq 0 3 [2, 3] @?= [2, 3],
    subSeq 1 1 [2, 3] @?= [],
    subSeq 1 2 [2, 3] @?= [3],
    subSeq 1 3 [2, 3] @?= [3],
    subSeq 0 2 [2, 3] @?= [2, 3],
    subSeq 1 3 [2, 3, 5] @?= [3, 5],
    subSeq 0 3 [2, 3, 5] @?= [2, 3, 5]]

testSubSubSeq = "subSubSeq" ~:: [
    subSubSeq 0 0 [[]] @?= ([[]] :: [[Int]]),
    subSubSeq 0 0 [[2]] @?= [[]],
    subSubSeq 0 1 [[2]] @?= [[2]],
    subSubSeq 0 0 [[2, 3]] @?= [[]],
    subSubSeq 0 1 [[2, 3]] @?= [[2]],
    subSubSeq 0 2 [[2, 3]] @?= [[2, 3]],
    subSubSeq 1 1 [[2, 3]] @?= [[]],
    subSubSeq 1 2 [[2, 3]] @?= [[3]],
    subSubSeq 1 3 [[2, 3]] @?= [[3]],
    subSubSeq 0 2 [[2, 3]] @?= [[2, 3]],
    subSubSeq 1 3 [[2, 3, 5]] @?= [[3, 5]],
    subSubSeq 0 3 [[2, 3, 5]] @?= [[2, 3, 5]],
    subSubSeq 0 0 [[2, 3], [5, 7]] @?= [[2, 3], []],
    subSubSeq 0 1 [[2, 3], [5, 7]] @?= [[2, 3], [5]],
    subSubSeq 0 2 [[2, 3], [5, 7]] @?= [[2, 3], [5, 7]],
    subSubSeq 1 0 [[2, 3], [5, 7]] @?= [[3], []],
    subSubSeq 1 1 [[2, 3], [5, 7]] @?= [[3], [5]],
    subSubSeq 1 2 [[2, 3], [5, 7]] @?= [[3], [5, 7]],
    subSubSeq 2 0 [[2, 3], [5, 7]] @?= [[], []],
    subSubSeq 2 1 [[2, 3], [5, 7]] @?= [[], [5]],
    subSubSeq 2 2 [[2, 3], [5, 7]] @?= [[], [5, 7]]]

testGroupByIndex = "groupByIndex" ~:: [
    groupByIndex 0 [(0, 2)] @?= [],
    groupByIndex 1 [(0, 2)] @?= [[2]],
    groupByIndex 2 [(0, 2)] @?= [[2], []],
    groupByIndex 0 [(1, 2)] @?= [],
    groupByIndex 1 [(1, 2)] @?= [[]],
    groupByIndex 2 [(1, 2)] @?= [[], [2]],
    groupByIndex 3 [(1, 2)] @?= [[], [2], []],
    groupByIndex 0 [(0, 2), (0, 3)] @?= [],
    groupByIndex 1 [(0, 2), (0, 3)] @?= [[3, 2]],
    groupByIndex 1 [(0, 2), (1, 3)] @?= [[2]],
    groupByIndex 1 [(1, 2), (1, 3)] @?= [[]],
    groupByIndex 2 [(0, 2), (0, 3)] @?= [[3, 2], []],
    groupByIndex 2 [(0, 2), (1, 3)] @?= [[2], [3]],
    groupByIndex 2 [(1, 2), (1, 3)] @?= [[], [3, 2]],
    groupByIndex 3 [(0, 2), (0, 3)] @?= [[3, 2], [], []],
    groupByIndex 3 [(0, 2), (1, 3)] @?= [[2], [3], []],
    groupByIndex 3 [(1, 2), (1, 3)] @?= [[], [3, 2], []],
    groupByIndex 5 [(0, 2), (2, 5), (2, 3), (4, 13), (4, 11), (4, 7)] @?= [[2], [], [3, 5], [], [7, 11, 13]]]
