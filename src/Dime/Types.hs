{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}


module Dime.Types where


import           Control.Lens        hiding (at, (.=))
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Types    (Parser)
import           Data.ByteString     (ByteString)
import           Data.Data
import qualified Data.Text           as T
import           Data.Text.Encoding
import           Data.Time
import           Database.Persist.TH
import           GHC.Generics

import           Dime.Types.Fields


unString :: MonadPlus m => Value -> m T.Text
unString (Object _) = mzero
unString (Array  _) = mzero
unString (String s) = return s
unString (Number _) = mzero
unString (Bool   _) = mzero
unString Null       = mzero

bsKey :: Object -> T.Text -> Parser ByteString
bsKey o k = fmap encodeUtf8 . unString =<< o .: k

type UserName = T.Text

data ConsumerKey
    = CKey
        { _ckeyKey    :: !ByteString
        , _ckeySecret :: !ByteString
        }
        deriving (Show, Eq, Data, Typeable, Generic)
$(makeLenses ''ConsumerKey)

instance FromJSON ConsumerKey where
    parseJSON (Object v) = CKey <$> bsKey v "key" <*> bsKey v "secret"
    parseJSON _          = mzero

instance ToJSON ConsumerKey where
    toJSON (CKey k s) = object [ "key"    .= decodeUtf8 k
                               , "secret" .= decodeUtf8 s
                               ]

data TwitterToken
    = TToken
        { _ttokenToken  :: !ByteString
        , _ttokenSecret :: !ByteString
        }
        deriving (Show, Eq, Data, Typeable, Generic)
$(makeLenses ''TwitterToken)

instance FromJSON TwitterToken where
    parseJSON (Object v) = TToken <$> bsKey v "token" <*> bsKey v "secret"
    parseJSON _          = mzero

instance ToJSON TwitterToken where
    toJSON (TToken t s) = object [ "token"  .= decodeUtf8 t
                                 , "secret" .= decodeUtf8 s
                                 ]

data TwitterLoginInfo
    = TwitterLoginInfo
    { _loginKey   :: !ConsumerKey
    , _loginToken :: !(Maybe TwitterToken)
    }
    deriving (Show, Eq, Data, Typeable, Generic)
$(makeLenses ''TwitterLoginInfo)

instance FromJSON TwitterLoginInfo where
    parseJSON (Object v) = TwitterLoginInfo <$> v .: "key" <*> v .:? "token"
    parseJSON _          = mzero

instance ToJSON TwitterLoginInfo where
    toJSON (TwitterLoginInfo k t) = object [ "key"   .= k
                                           , "token" .= t
                                           ]

data LoginInfo
    = LoginInfo
    { _loginTwitter :: !(Maybe TwitterLoginInfo)
    , _loginGmail   :: !(Maybe ByteString)
    }
$(makeLenses ''LoginInfo)

instance FromJSON LoginInfo where
    parseJSON (Object v) =   LoginInfo
                         <$> v .:? "twitter"
                         <*> (fmap encodeUtf8 <$> v .:? "gmail")
    parseJSON _          = mzero

instance ToJSON LoginInfo where
    toJSON (LoginInfo t g) = object [ "twitter" .= t
                                    , "gmail"   .= fmap decodeUtf8 g
                                    ]

data IdCursor
    = NotStarted
    | Cursor Integer
    | CursorDone
  deriving (Eq, Show, Data, Typeable, Generic)

cursorDoneMaybe :: IdCursor -> Maybe Integer
cursorDoneMaybe (Cursor i) = Just i
cursorDoneMaybe _          = Nothing

isCursorDone :: IdCursor -> Bool
isCursorDone CursorDone = True
isCursorDone _          = False

share [mkPersist (sqlSettings { mpsGenerateLenses = True }), mkMigrate "migrateAll"]
    [persistLowerCase|
Post json
    source PostSource
    sourceId T.Text
    sender T.Text
    message T.Text
    raw (JSONField PostObject)
    sent UTCTime
    created UTCTime default=CURRENT_TIME
    UniquePostSourceId sourceId
    deriving Show Eq

DownloadCache json
    url String
    params T.Text
    source PostSource
    postId PostId Maybe
    created UTCTime default=CURRENT_TIME
    UniqueDownloadUrl url params
    deriving Show Eq
|]
