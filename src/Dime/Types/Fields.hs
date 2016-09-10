{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TemplateHaskell    #-}


module Dime.Types.Fields where


import           Control.Lens
import           Data.Aeson
import           Data.Bifunctor         (first)
import           Data.ByteString.Lazy   (toStrict)
import           Data.Data
import           Data.Monoid            ((<>))
import qualified Data.Text              as T
import           Database.Persist.Class
import           Database.Persist.Sql
import           Database.Persist.TH
import           GHC.Generics           hiding (to)
import           Web.Twitter.Types

import           Dime.Types.Google


-- * PostSource

data PostSource = Gmail | Twitter
                deriving (Read, Show, Eq, Data, Typeable, Generic)
derivePersistField "PostSource"
$(makePrisms ''PostSource)

instance ToJSON PostSource where
    toJSON     = genericToJSON defaultOptions
    toEncoding = genericToEncoding defaultOptions

instance FromJSON PostSource

-- * JSONField

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

-- * PostObject

data PostObject
    = PostGmail   Message
    | PostTwitter DirectMessage
    deriving (Show, Eq, Data, Typeable, Generic)
$(makePrisms ''PostObject)

instance ToJSON PostObject where
    toJSON     = genericToJSON     defaultOptions
    toEncoding = genericToEncoding defaultOptions

instance FromJSON PostObject
