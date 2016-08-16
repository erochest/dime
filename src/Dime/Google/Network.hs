{-# LANGUAGE OverloadedStrings #-}


module Dime.Google.Network where


import           Control.Error
import           Control.Lens          hiding ((??))
import           Control.Monad.Reader
import           Data.Aeson
import qualified Data.ByteString.Char8 as B8
import           Network.HTTP.Conduit  hiding (Proxy, responseBody)
import           Network.OAuth.OAuth2
import           Network.Wreq

import           Dime.Config
import           Dime.Types


getJSON :: FromJSON a => URI -> Google a
getJSON uri = do
    (m, t) <- ask
    liftG $ authGetJSON m t uri

postJSON :: (ToJSON a, FromJSON b) => URI -> a -> Google b
postJSON uri d = do
    (m, t) <- ask
    let opts = defaults
             & manager .~ Right m
             & auth ?~ oauth2Bearer (accessToken t)
    fmap (view responseBody) . asJSON
        =<< liftIO (postWith opts (B8.unpack uri) $ toJSON d)

googleOAuth :: OAuth2
googleOAuth = OAuth2 "994279088207-aicibrrd9vonnfbbk1gcpskvb5qfnn7h\
                     \.apps.googleusercontent.com"
                     "B6WSP2OTZso-QO9joatmKcU3"
                     "https://accounts.google.com/o/oauth2/auth"
                     "https://accounts.google.com/o/oauth2/token"
                     (Just "urn:ietf:wg:oauth:2.0:oob")

runGoogle' :: FilePath -> Google a -> Script a
runGoogle' configFile a = withConfig' configFile $ \config -> do
    refresh <- (config ^? loginGmail . _Just)
            ?? "You must call 'dime google-login'."
    m       <- scriptIO $ newManager tlsManagerSettings
    token'  <- liftSG $  fetchRefreshToken m googleOAuth refresh
    runGoogle m token' a
