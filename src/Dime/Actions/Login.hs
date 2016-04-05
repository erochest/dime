{-# LANGUAGE OverloadedStrings #-}


module Dime.Actions.Login where


import           Control.Error
import           Control.Lens                 hiding ((??))
import           Control.Monad                (void)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Data.Aeson
import qualified Data.ByteString              as B
import qualified Data.ByteString.Char8        as B8
import qualified Data.ByteString.Lazy         as BL
import           Network.HTTP.Conduit
import           System.IO
import           Web.Authenticate.OAuth
import           Web.Browser
import           Web.Twitter.Conduit

import           Dime.Types


putStrLn' :: String -> Script ()
putStrLn' = scriptIO . putStrLn

loginTwitter :: FilePath -> Script ()
loginTwitter configFile = do
    config <-  (  hoistEither . eitherDecodeStrict'
               =<< scriptIO (B.readFile configFile)
               ) :: Script LoginInfo

    let oauth = twitterOAuth
                { oauthConsumerKey    = config ^. loginKey
                , oauthConsumerSecret = config ^. loginSecret
                , oauthCallback       = Just "oop"
                }
    manager <- scriptIO $ newManager tlsManagerSettings
    Credential cred <- scriptIO . runResourceT $ do
        -- TODO: Appears to fail on this line. Not sure why.
        cred <- getTemporaryCredential oauth manager
        let url = authorizeUrl oauth cred
        pin <- getPIN url
        getAccessToken oauth (insert "oauth_verifier" pin cred) manager

    up <- (.) <$> lookUpdate "oauth_token"        loginToken       cred
              <*> lookUpdate "oauth_token_secret" loginTokenSecret cred
    scriptIO . BL.writeFile configFile . encode $ up config

lookup' :: (Eq a1, Show a1) => a1 -> [(a1, a)] -> Script a
lookup' k xs = Prelude.lookup k xs ?? ("Missing " ++ show k)

lookUpdate :: (Eq a2, Show a2)
           => a2 -> ASetter s t a (Maybe a1) -> [(a2, a1)] -> Script (s -> t)
lookUpdate k f xs = set f . Just <$> lookup' k xs

getPIN :: String -> ResourceT IO B.ByteString
getPIN url = liftIO $ do
    void $ openBrowser url
    putStr "> what was the PIN twitter provided you with? "
    hFlush stdout
    B8.getLine
