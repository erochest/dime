{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}


module Dime.Types where


import           Control.Arrow              ((&&&))
import           Control.Error              (ExceptT (..), Script, throwE)
import           Control.Lens               hiding (at, (.=))
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Acid
import           Data.Aeson
import           Data.Aeson.Types           (Parser)
import           Data.Bifunctor             (first)
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Data
import           Data.SafeCopy
import qualified Data.Text                  as T
import           Data.Text.Encoding
import           GHC.Generics
import           Network.HTTP.Client        (Manager)
import           Network.OAuth.OAuth2
import           Web.Twitter.Types


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

$(deriveSafeCopy 0 'base ''IdCursor)

cursorDoneMaybe :: IdCursor -> Maybe Integer
cursorDoneMaybe (Cursor i) = Just i
cursorDoneMaybe _          = Nothing

isCursorDone :: IdCursor -> Bool
isCursorDone CursorDone = True
isCursorDone _          = False

type CursorData = (IdCursor, [DirectMessage])

data DMCursor
    = DMCursor
    { _cursorMaxId :: !IdCursor
    , _cursorDMs   :: ![DirectMessage]
    } deriving (Show, Eq, Data, Typeable, Generic)
$(makeLenses ''DMCursor)

$(deriveSafeCopy 0 'base ''Coordinates)
$(deriveSafeCopy 0 'base ''User)
$(deriveSafeCopy 0 'base ''DirectMessage)
$(deriveSafeCopy 0 'base ''DMCursor)

writeDMCursor :: CursorData -> Update DMCursor ()
writeDMCursor = put . uncurry DMCursor

queryDMCursor :: Query DMCursor CursorData
queryDMCursor = (view cursorMaxId &&& view cursorDMs) <$> ask

$(makeAcidic ''DMCursor ['writeDMCursor, 'queryDMCursor])

newtype Google a
    = Google { unGoogle :: ReaderT (Manager, AccessToken) Script a }
    deriving ( Functor, Applicative, Monad, MonadReader (Manager, AccessToken)
             , MonadIO, Generic
             )

instance MonadThrow Google where
    throwM = Google . lift . throwE . show

runGoogle :: Manager -> AccessToken -> Google a -> Script a
runGoogle m at g = runReaderT (unGoogle g) (m, at)

currentManager :: Google Manager
currentManager = asks fst

currentAccessToken :: Google AccessToken
currentAccessToken = asks snd

liftE :: Script a -> Google a
liftE = Google . lift

liftG :: IO (OAuth2Result a) -> Google a
liftG = liftE . liftSG

liftSG :: IO (OAuth2Result a) -> Script a
liftSG = ExceptT . fmap (first L8.unpack)
