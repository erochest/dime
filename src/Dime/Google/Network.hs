{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}


module Dime.Google.Network where


import           Control.Concurrent
import           Control.Error
import           Control.Exception          (displayException)
import           Control.Lens               hiding ((??))
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Data.Aeson
import qualified Data.ByteString.Char8      as B8
import qualified Data.ByteString.Lazy.Char8 as BL8
import           Data.Foldable
import qualified Data.Text                  as T
import           Database.Persist.Sqlite
import           Network.HTTP.Conduit       hiding (Proxy, responseBody)
import           Network.OAuth.OAuth2
import           Network.Wreq
import           Network.Wreq.Types         hiding (auth, manager)

import           Dime.Config
import           Dime.Google.Types
import           Dime.Types
import           Dime.Utils


requestWait :: Int
requestWait = 2000

baseURL :: URI
baseURL = "https://www.googleapis.com"

normURL :: URI -> String
normURL = B8.unpack . mappend baseURL

asJSON' :: FromJSON a => Response BL8.ByteString -> Google a
asJSON' = liftE
        . hoistEither
        . (displayException `bimap` view responseBody)
        . asJSON

getJSON :: FromJSON a => URI -> Google a
getJSON uri = getJSON' uri []

getJSON' :: FromJSON a => URI -> [GetParam] -> Google a
getJSON' uri ps = do
    (m, t) <- currentManagerToken
    let opts' = defaults
              & manager .~ Right m
              & auth ?~ oauth2Bearer (accessToken t)
        opts  = foldl' setp opts' ps
    liftIO . threadDelay =<< watchM "waiting " requestWait
    asJSON' =<< liftIO (getWith opts (normURL uri))

postJSON :: (Postable a, FromJSON b) => URI -> a -> Google b
postJSON uri d = do
    (m, t) <- currentManagerToken
    let opts = defaults
             & manager .~ Right m
             & auth ?~ oauth2Bearer (accessToken t)
    asJSON' =<< liftIO (postWith opts (normURL uri) d)

postJSON' :: (Postable a, FromJSON b)
          => URI -> [GetParam] -> a -> Google b
postJSON' uri ps d = do
    (m, t) <- currentManagerToken
    let opts' = defaults
              & manager .~ Right m
              & auth ?~ oauth2Bearer (accessToken t)
        opts  = foldl' setp opts' ps
    asJSON' =<< liftIO (postWith opts (normURL uri) d)

googleOAuth :: OAuth2
googleOAuth = OAuth2 "994279088207-aicibrrd9vonnfbbk1gcpskvb5qfnn7h\
                     \.apps.googleusercontent.com"
                     "B6WSP2OTZso-QO9joatmKcU3"
                     "https://accounts.google.com/o/oauth2/auth"
                     "https://accounts.google.com/o/oauth2/token"
                     (Just "urn:ietf:wg:oauth:2.0:oob")

runGoogle' :: FilePath -> FilePath -> Google a -> Script a
runGoogle' configFile workingDb a =
    withConfig' configFile $ \config -> runStderrLoggingT $ do
        refresh <- lift $  (config ^? loginGmail . _Just)
                        ?? "You must call 'dime google-login'."
        m       <- lift . scriptIO $ newManager tlsManagerSettings
        token'  <- lift . liftSG   $ fetchRefreshToken m googleOAuth refresh

        let workingDb' = T.pack workingDb
        runSqlite workingDb' $
            runMigration migrateAll
        withSqliteConn workingDb' $ \s ->
            runGoogleL m token' s a

maybeParam :: Show a => T.Text -> Maybe a -> Maybe GetParam
maybeParam n = fmap ((n,) . pure . T.pack . show)

maybeParamT :: T.Text -> Maybe T.Text -> Maybe GetParam
maybeParamT n = fmap ((n,) . pure)

maybeList :: T.Text -> [T.Text] -> Maybe GetParam
maybeList _ [] = Nothing
maybeList n vs = Just (n, vs)

setp :: Options -> GetParam -> Options
setp o (n, vs) = o & param n .~ vs
