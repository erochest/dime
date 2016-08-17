{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}


module Dime.Google.Network where


import           Control.Error
import           Control.Lens          hiding ((??))
import           Control.Monad.Reader
import           Data.Aeson
import qualified Data.ByteString.Char8 as B8
import           Data.Foldable
import qualified Data.Text             as T
import           Network.HTTP.Conduit  hiding (Proxy, responseBody)
import           Network.OAuth.OAuth2
import           Network.Wreq
import           Network.Wreq.Types    hiding (auth, manager)

import           Dime.Config
import           Dime.Types


getJSON :: FromJSON a => URI -> Google a
getJSON uri = do
    (m, t) <- ask
    liftG $ authGetJSON m t uri

getJSON' :: FromJSON a => URI -> [(T.Text, [T.Text])] -> Google a
getJSON' uri ps = do
    (m, t) <- ask
    let opts' = defaults
              & manager .~ Right m
              & auth ?~ oauth2Bearer (accessToken t)
        opts  = foldl' setp opts' ps
    fmap (view responseBody) . asJSON =<< liftIO (getWith opts (B8.unpack uri))

postJSON :: (Postable a, FromJSON b) => URI -> a -> Google b
postJSON uri d = do
    (m, t) <- ask
    let opts = defaults
             & manager .~ Right m
             & auth ?~ oauth2Bearer (accessToken t)
    fmap (view responseBody) . asJSON
        =<< liftIO (postWith opts (B8.unpack uri) d)

postJSON' :: (Postable a, FromJSON b)
          => URI -> [(T.Text, [T.Text])] -> a -> Google b
postJSON' uri ps d = do
    (m, t) <- ask
    let opts' = defaults
              & manager .~ Right m
              & auth ?~ oauth2Bearer (accessToken t)
        opts  = foldl' setp opts' ps
    fmap (view responseBody) . asJSON
        =<< liftIO (postWith opts (B8.unpack uri) d)

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

maybeParam :: Show a => T.Text -> Maybe a -> Maybe (T.Text, [T.Text])
maybeParam n = fmap ((n,) . pure . T.pack . show)

maybeList :: T.Text -> [T.Text] -> Maybe (T.Text, [T.Text])
maybeList _ [] = Nothing
maybeList n vs = Just (n, vs)

setp :: Options -> (T.Text, [T.Text]) -> Options
setp o (n, vs) = o & param n .~ vs
