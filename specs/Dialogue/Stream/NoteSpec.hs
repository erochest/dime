{-# LANGUAGE OverloadedStrings #-}


module Dialogue.Stream.NoteSpec where


import           Data.Time
import           Test.Hspec

import           Dialogue.Streams.Note (parseNoteContent, parseNoteDate)


spec :: Spec
spec = do
    describe "parseNoteDate" $ do
        it "should parse a plain file name." $
            "april_13__2016_at_0455pm.txt" `shouldParseTo` "2016-04-13T16:55:00"

        it "should offset a duplicate file name." $
            "june_30__2016_at_1211pm (1).txt" `shouldParseTo`
                "2016-06-30T12:11:06"

    describe "parseNoteContent" $
        it "should only return the part before the break." $
            let input = "Some text here.\n\
                        \- - -\n\
                        \via http://google.com/\n"
            in  parseNoteContent input `shouldBe` "Some text here.\n"

parseTime' :: Monad m => String -> m UTCTime
parseTime' =
    parseTimeM False defaultTimeLocale (iso8601DateFormat $ Just "%H:%M:%S")

shouldParseTo :: String -> String -> Expectation
shouldParseTo actual expected = do
    a <- parseNoteDate actual
    e <- parseTime' expected
    a `shouldBe` e

-- april_13__2016_at_0455pm.txt
-- april_17__2016_at_0511pm.txt
-- april_27__2016_at_0323pm.txt
-- august_16__2016_at_1017pm.txt
-- august_21__2016_at_0929pm.txt
-- july_09__2016_at_0710pm.txt
-- july_14__2016_at_0848am.txt
-- july_20__2016_at_1131am.txt
-- june_13__2016_at_0849pm.txt
-- september_19__2016_at_0946am.txt

-- april_09__2016_at_1049am (1).txt
-- april_30__2016_at_0716am (1).txt
-- august_22__2016_at_0156pm (1).txt
-- august_26__2016_at_0640am (1).txt
-- august_26__2016_at_0641am (1).txt
-- july_16__2016_at_0742pm (1).txt
-- july_16__2016_at_0743pm (1).txt
-- july_30__2016_at_1015pm (1).txt
-- june_30__2016_at_1211pm (1).txt
-- september_06__2016_at_0759pm (1).txt
