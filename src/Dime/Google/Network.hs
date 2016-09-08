{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}


module Dime.Google.Network where


import           Control.Error
import           Control.Lens               hiding ((??))
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Data.Aeson
import           Data.Foldable
import qualified Data.Text                  as T
import           Database.Persist.Sqlite
import           Network.HTTP.Conduit       hiding (Proxy, responseBody)
import           Network.OAuth.OAuth2
import           Network.Wreq
import           Network.Wreq.Types         hiding (auth, manager)

import           Dime.Config
import           Dime.Google.Network.Utils
import           Dime.Google.Types
import           Dime.Types


getJSON :: FromJSON a => URI -> Google a
getJSON uri = getJSON' uri []

-- TODO: Get or create an ArchiveSession (from a StateT)
-- TODO: queueURL
-- TODO: getCacheURL
getJSON' :: FromJSON a => URI -> [GetParam] -> Google a
getJSON' = getUncachedJSON

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
