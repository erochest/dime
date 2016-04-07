{-# LANGUAGE OverloadedStrings #-}


module Dime.Actions.Login where


import           Control.Error
import           Control.Lens                 hiding ((??))
import           Control.Monad                (void)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import qualified Data.ByteString              as B
import qualified Data.ByteString.Char8        as B8
import           Network.HTTP.Conduit
import           System.IO
import           Web.Authenticate.OAuth
import           Web.Browser
import           Web.Twitter.Conduit

import           Dime.Config
import           Dime.Types


loginTwitter :: FilePath -> Script ()
loginTwitter configFile = do
    config <- readConfig configFile

    let oauth = twitterOAuth
                { oauthConsumerKey    = config ^. loginKey . ckeyKey
                , oauthConsumerSecret = config ^. loginKey . ckeySecret
                , oauthCallback       = Just "oob"
                }
    manager <- scriptIO $ newManager tlsManagerSettings
    Credential cred <- scriptIO . runResourceT $ do
        cred <- getTemporaryCredential oauth manager
        let url = authorizeUrl oauth cred
        pin <- getPIN url
        getAccessToken oauth (insert "oauth_verifier" pin cred) manager

    ttoken <-  TToken
           <$> Prelude.lookup "oauth_token" cred ?? "Missing oauth_token."
           <*> Prelude.lookup "oauth_token_secret" cred
                                        ?? "Missing oauth_token_secret."

    writeConfig configFile . (loginToken .~ Just ttoken) $ config

getPIN :: String -> ResourceT IO B.ByteString
getPIN url = liftIO $ do
    void $ openBrowser url
    putStr "> what was the PIN twitter provided you with? "
    hFlush stdout
    B8.getLine
