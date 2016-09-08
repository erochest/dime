{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TemplateHaskell    #-}


module Dime.Types.Fields where


import qualified Codec.Binary.QuotedPrintable as QP
import           Control.Applicative
import           Control.Error
import           Control.Lens
import           Data.Aeson
import           Data.Bifunctor               (first)
import           Data.ByteString.Lazy         (toStrict)
import           Data.Data
import           Data.Foldable
import           Data.Monoid
import qualified Data.Text                    as T
import           Data.Text.Encoding
import           Data.Time
import           Data.Time.Clock.POSIX
import           Database.Persist.Class
import           Database.Persist.Sql
import           Database.Persist.TH
import           GHC.Generics                 hiding (to)
import           Web.Twitter.Types.Lens

import           Dime.Google.Types
import           Dime.Utils


data PostSource = Gmail | Twitter
                deriving (Read, Show, Eq, Data, Typeable, Generic)
derivePersistField "PostSource"
$(makePrisms ''PostSource)

instance ToJSON PostSource where
    toJSON     = genericToJSON defaultOptions
    toEncoding = genericToEncoding defaultOptions

instance FromJSON PostSource

data PostObject
    = PostGmail   Message
    | PostTwitter DirectMessage
    deriving (Show, Eq, Data, Typeable, Generic)
$(makePrisms ''PostObject)

class ToPostObject o where
    toPostObject   :: o -> PostObject
    fromPostObject :: PostObject -> Maybe o
    getSource      :: o -> PostSource
    getSourceId    :: o -> T.Text
    getSender      :: o -> Maybe T.Text
    getMessage     :: o -> Maybe T.Text
    getMetadata    :: o -> [GetParam]
    getSent        :: o -> Maybe UTCTime

instance ToPostObject Message where
    toPostObject = PostGmail
    fromPostObject (PostGmail   m) = Just m
    fromPostObject (PostTwitter _) = Nothing

    getSource    = const Gmail
    getSourceId  = view messageId
    getSender    = fmap mconcat . lookup "From" . getMetadata
    getMessage m =   ( pl ^? payloadBody . attachmentData . _Just
                     . to unBytes . to decodeUtf8)
                 <|> (   hush . fmap decodeUtf8 . QP.decode . unBytes
                     =<< preview (payloadBody . attachmentData . _Just)
                     =<< listToMaybe . filter isTextPart
                     =<< pl ^. payloadParts
                     )
                 where
                     pl = m ^. messagePayload
                     isTextPart :: Payload -> Bool
                     isTextPart = any isTextHeader . fold . _payloadHeaders
                     isTextHeader :: Header -> Bool
                     isTextHeader Header{..} =  _headerName  == "Content-Type"
                                             && _headerValue == "text/plain; charset=utf-8"
    getMetadata  = fmap headerToParam
                 . fold
                 . view (messagePayload . payloadHeaders)
    getSent      = hush
                 . fmap (posixSecondsToUTCTime . realToFrac)
                 . (decimalE :: T.Text -> Either String Integer)
                 . view messageInternalDate

instance ToPostObject DirectMessage where
    toPostObject   = PostTwitter
    fromPostObject (PostGmail   _) = Nothing
    fromPostObject (PostTwitter m) = Just m

    getSource      = const Twitter
    getSourceId    = T.pack . show . view dmId
    getSender      = Just . view dmSenderScreenName
    getMessage     = Just . view dmText
    getMetadata dm = [ ("From", [dm ^. dmSenderScreenName])
                     , ("To"  , [dm ^. dmRecipientScreeName])
                     , ("Date", [rfc822Date $ dm ^. dmCreatedAt])
                     , ("ID"  , [T.pack . show $ dm ^. dmId])
                     ]
    getSent        = Just . view dmCreatedAt

instance ToJSON PostObject where
    toJSON     = genericToJSON     defaultOptions
    toEncoding = genericToEncoding defaultOptions

instance FromJSON PostObject

newtype JSONField a = JSONField { unJSON :: a }
                    deriving (Show, Eq, Data, Typeable, Generic)

instance ToJSON a => ToJSON (JSONField a) where
    toJSON     = toJSON     . unJSON
    toEncoding = toEncoding . unJSON

instance FromJSON a => FromJSON (JSONField a) where
    parseJSON = fmap JSONField . parseJSON

instance (ToJSON a, FromJSON a) => PersistField (JSONField a) where
    toPersistValue = PersistByteString . toStrict . encode . unJSON
    fromPersistValue (PersistByteString b) = first T.pack $ eitherDecodeStrict b
    fromPersistValue p = Left $ "Invalid PersistValue: " <> T.pack (show p)

instance (ToJSON a, FromJSON a) => PersistFieldSql (JSONField a) where
    sqlType _ = SqlBlob
