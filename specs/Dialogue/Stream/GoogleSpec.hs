{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}


module Dialogue.Stream.GoogleSpec where


import           Control.Error
import           Control.Lens               hiding ((...))
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Foldable
import           Data.Text.Encoding
import           Data.Time
import           Network.Wreq

import           Test.Hspec

import           Dialogue.Streams.Google
import           Dialogue.Types.Dialogue


spec :: Spec
spec = do
    let contentType = "multipart/mixed; boundary=batch_endW4uAcPQU_AAMiKCgyOAE"
        jsonMsg :: Response L8.ByteString -> Dialogue Message
        jsonMsg = asJSON'

    describe "parseMulti'" $
        it "should return all messages." $ do
            body <- L8.readFile "./specs/batch-response-00.txt"
            rs   <- runDialogueTest (return ()) $ parseMulti' contentType body
            (length <$> hush rs) `shouldBe` Just 32

    describe "decodeContent" $
        it "should decode the payload.body.data properly." $ do
            body <- L8.readFile "./specs/batch-response-00.txt"
            rs   <-  fmap fold
                 .   runDialogueTest (return ())
                 $   mapM (hoistE . decodeContent . encodeUtf8)
                 .   toListOf ( traverse . messagePayload . payloadBody
                              . attachmentData . _Just)
                 .   take 5
                 =<< mapM jsonMsg
                 =<< parseMulti' contentType body
            rs `shouldBe` [ "Itaque magni perferendis laboriosam vitae."
                          , "In maxime at exercitationem assumenda nesciunt."
                          , "Magni minima quas perspiciatis vero."
                          , "Incidunt praesentium aliquid nulla architecto eum optio."
                          , "Molestiae velit asperiores neque veniam cumque adipisci."
                          ]

    describe "decodeDate" $
        it "should parse the internal message date into the right value." $
            hush (   formatTime defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%S"))
                 <$> decodeDate "1475073286244")
                `shouldBe` Just "2016-09-28T14:34:46"
