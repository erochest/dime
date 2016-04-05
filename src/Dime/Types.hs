{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}


module Dime.Types where


import           Control.Applicative
import           Control.Lens        hiding ((.=))
import           Control.Monad
import           Data.Aeson
import           Data.ByteString     (ByteString)
import           Data.Data
import           Data.Text.Encoding
import           GHC.Generics


-- TODO: The type here should really group key & secret and
-- token & token_secret so that if each pair is set, both are.
data LoginInfo
    = LoginInfo
        { _loginKey         :: !ByteString
        , _loginSecret      :: !ByteString
        , _loginToken       :: !(Maybe ByteString)
        , _loginTokenSecret :: !(Maybe ByteString)
        }
        deriving (Show, Eq, Data, Typeable, Generic)
$(makeLenses ''LoginInfo)

instance FromJSON LoginInfo where
    parseJSON (Object v) =   LoginInfo
                         <$> bsKey v "key"
                         <*> bsKey v "secret"
                         <*> optional (bsKey v "token")
                         <*> optional (bsKey v "token_secret")
                             where
                                 bsKey o k = fmap encodeUtf8 . unString =<< o .: k
                                 unString (Object _) = mzero
                                 unString (Array  _) = mzero
                                 unString (String s) = return s
                                 unString (Number _) = mzero
                                 unString (Bool   _) = mzero
                                 unString Null       = mzero
    parseJSON _          = mzero

instance ToJSON LoginInfo where
    toJSON (LoginInfo k s t ts)
      = object [ "key"          .= bs (Just k)
               , "secret"       .= bs (Just s)
               , "token"        .= bs t
               , "token_secret" .= bs ts
               ]
      where
          bs (Just st) = String $ decodeUtf8 st
          bs Nothing   = Null
