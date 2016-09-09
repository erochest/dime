{-# LANGUAGE OverloadedStrings #-}


module Dime.Google where


import           Control.Error
import           Control.Lens              hiding ((??))
import           Control.Monad.Logger
import           Control.Monad.Trans.Class
import           Data.Aeson
import           Data.Aeson.Lens
import qualified Data.Text                 as T
import           Database.Persist.Sqlite
import           Network.HTTP.Conduit
import           Network.OAuth.OAuth2

import           Dime.Config
import           Dime.Network.Utils
import           Dime.Types
import           Dime.Types.Fields


getUser :: Dime UserName
getUser =   liftE
        .   (?? "Invalid user profile response.")
        .   preview (key "emailAddress". _String)
        =<< (getUncachedJSON url [] :: Dime Value)
    where
        url = "/gmail/v1/users/me/profile"

googleOAuth :: OAuth2
googleOAuth = OAuth2 "994279088207-aicibrrd9vonnfbbk1gcpskvb5qfnn7h\
                     \.apps.googleusercontent.com"
                     "B6WSP2OTZso-QO9joatmKcU3"
                     "https://accounts.google.com/o/oauth2/auth"
                     "https://accounts.google.com/o/oauth2/token"
                     (Just "urn:ietf:wg:oauth:2.0:oob")

runGoogle' :: FilePath -> FilePath -> Dime a -> Script a
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
            runDimeL Gmail m token' s a
