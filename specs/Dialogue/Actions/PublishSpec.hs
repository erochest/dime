{-# LANGUAGE OverloadedStrings #-}


module Dialogue.Actions.PublishSpec where


import           Data.Foldable
import           Data.Monoid
import qualified Data.Text                as T
import           Data.Time

import           Test.Hspec

import           Dialogue.Actions.Publish
import           Dialogue.Fields
import           Dialogue.Types.Publish


mkt :: Integer -> Int -> Int -> DiffTime -> DiffTime -> DiffTime -> UTCTime
mkt year month day hour minute second =
    UTCTime (fromGregorian year month day)
            (hour * 60 * 60 + minute * 60 + second)

mkb :: T.Text -> Integer -> Int -> Int -> DiffTime -> DiffTime -> DiffTime
    -> PublishBlock
mkb n year month day hour minute second =
    PublishBlock n True AdiumService (mkt year month day hour minute second) ""
                 mempty

spec :: Spec
spec =
    describe "groupBlocks" $ do
        let pba = mkb "a" 2016 10 11 11 10 00
            pbb = mkb "b" 2016 10 11 11 20 00
            pbc = mkb "c" 2016 10 11 11 30 00
            pbd = mkb "d" 2016 10 11 11 40 00
            pbe = mkb "e" 2016 10 11 11 50 00
            pbf = mkb "f" 2016 10 11 13 00 00
            pbg = mkb "g" 2016 10 11 13 10 00
            pbh = mkb "h" 2016 10 11 13 20 00

            d15 = 15 * 60

        it "should group closely occurring blocks into one GroupBlock." $ do
            let group = groupBlocks d15 [pba, pbb, pbc, pbd]
            group `shouldSatisfy` ((== 1) . length)
            getSum (foldMap (Sum . length . toList . _bgBlocks) group)
                `shouldSatisfy` (== 4)

        it "should group a spread of blocks into multiple GroupBlocks." $ do
            let group = groupBlocks d15 [pba, pbb, pbc, pbd, pbe, pbf, pbg, pbh]
            group `shouldSatisfy` ((== 2) . length)

        it "should group a spread of blocks with nearby blocks." $ do
            let group = groupBlocks d15 [pba, pbb, pbc, pbd, pbe, pbf, pbg, pbh]
            map (length . toList . _bgBlocks) group `shouldBe` [5, 3]
            map (map _pbName . toList . _bgBlocks) group
                `shouldBe` ([ ["a", "b", "c", "d", "e"]
                            , ["f", "g", "h"]
                            ] :: [[T.Text]])

        it "should set its start time from its contents." $ do
            let group = groupBlocks d15 [pba, pbb, pbc, pbd, pbe, pbf, pbg, pbh]
            map _bgStartTime group `shouldBe` [ mkt 2016 10 11 11 10 00
                                              , mkt 2016 10 11 13 00 00
                                              ]

        it "should set its end time from its contents." $ do
            let group = groupBlocks d15 [pba, pbb, pbc, pbd, pbe, pbf, pbg, pbh]
            map _bgEndTime group `shouldBe` [ mkt 2016 10 11 11 50 00
                                            , mkt 2016 10 11 13 20 00
                                            ]
