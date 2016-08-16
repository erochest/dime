{-# LANGUAGE OverloadedStrings #-}


module Dime.Google where


import           Control.Error
import           Control.Lens         hiding ((??))
import           Control.Monad.Reader
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.Monoid
import           Data.Text.Encoding
import           Network.HTTP.Conduit hiding (Proxy)
import           Network.OAuth.OAuth2

import           Dime.Config
import           Dime.Google.Types
import           Dime.Types


getUser :: Google UserName
getUser =   liftE
        .   (?? "Invalid user profile response.")
        .   preview (key "emailAddress". _String)
        =<< (getJSON url :: Google Value)
    where
        url = "https://www.googleapis.com/gmail/v1/users/me/profile"

listLabels :: Google [Label]
listLabels =   _labelsLabels
           <$> (getJSON url :: Google Labels)
    where
        url = "https://www.googleapis.com/gmail/v1/users/me/labels"

getLabel :: LabelId -> Google Label
getLabel lId =   undefined
             =<< (getJSON url :: Google Label)
    where
        url = "https://www.googleapis.com/gmail/v1/users/me/labels/" <> encodeUtf8 lId

getJSON :: FromJSON a => URI -> Google a
getJSON uri = do
    (m, t) <- ask
    liftG $ authGetJSON m t uri

-- TODO: Get this from the command line or environment
googleOAuth :: OAuth2
googleOAuth = OAuth2 "994279088207-aicibrrd9vonnfbbk1gcpskvb5qfnn7h\
                     \.apps.googleusercontent.com"
                     "B6WSP2OTZso-QO9joatmKcU3"
                     "https://accounts.google.com/o/oauth2/auth"
                     "https://accounts.google.com/o/oauth2/token"
                     (Just "urn:ietf:wg:oauth:2.0:oob")

runGoogle' :: FilePath -> Google a -> Script a
runGoogle' configFile a = withConfig' configFile $ \config -> do
    refresh <- (config ^? loginGmail . _Just) ?? "You must call 'dime google-login'."
    manager <- scriptIO $ newManager tlsManagerSettings
    token'  <- liftSG $  fetchRefreshToken manager googleOAuth refresh
    runGoogle manager token' a
