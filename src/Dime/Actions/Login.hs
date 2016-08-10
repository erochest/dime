{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Dime.Actions.Login where


import           Control.Applicative
import           Control.Arrow                hiding (first)
import           Control.Error
import           Control.Lens                 hiding ((??))
import           Control.Monad                (void)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Data.Bifunctor
import qualified Data.ByteString              as B
import qualified Data.ByteString.Char8        as B8
import qualified Data.ByteString.Lazy.Char8   as L8
import           Network.HTTP.Conduit         hiding (Proxy)
import           Network.OAuth.OAuth2
import           System.IO
import           Web.Authenticate.OAuth
import           Web.Browser
import           Web.Twitter.Conduit
import qualified Web.Twitter.Conduit          as C

import           Dime.Auth
import           Dime.Config
import           Dime.Types
import qualified Dime.Types                   as D


loginTwitter :: FilePath -> Maybe (B.ByteString, B.ByteString) -> Script ()
loginTwitter configFile keySecret = withConfig configFile $ \config -> do
    let tLogin = config ^. D.loginTwitter
    (tKey, tSecret) <- (   keySecret
                       <|> (   (view ckeyKey &&& view ckeySecret)
                           <$> tLogin ^? _Just . loginKey)
                       ) ??  "Missing Twitter key & secret."

    let oauth = twitterOAuth
              { oauthConsumerKey    = tKey -- tLogin ^. loginKey . ckeyKey
              , oauthConsumerSecret = tSecret -- tLogin ^. loginKey . ckeySecret
              , C.oauthCallback     = Just "oob"
              }
    manager <- scriptIO $ newManager tlsManagerSettings
    Credential cred <- scriptIO . runResourceT $ do
        cred <- getTemporaryCredential oauth manager
        let url = authorizeUrl oauth cred
        pin <- getPIN "Twitter" url
        getAccessToken oauth (insert "oauth_verifier" pin cred) manager

    ttoken <-  TToken
           <$> Prelude.lookup "oauth_token" cred ?? "Missing oauth_token."
           <*> Prelude.lookup "oauth_token_secret" cred
                                        ?? "Missing oauth_token_secret."

    return $ config
           & D.loginTwitter .~ Just (TwitterLoginInfo (CKey tKey tSecret)
                                                      $ Just ttoken)

getPIN :: String -> String -> ResourceT IO B.ByteString
getPIN provider url = liftIO $ do
    putStrLn $ "Opening " ++ provider ++ ": <" ++ url ++ ">"
    void $ openBrowser url
    putStr $ "> what was the PIN " ++ provider ++ " provided you with? "

    hFlush stdout
    B8.getLine

loginGmail :: FilePath -> Script ()
loginGmail configFile = withConfig configFile $ \config -> do
    -- TODO: Get this from the command line or environment
    let oauth = OAuth2 "994279088207-aicibrrd9vonnfbbk1gcpskvb5qfnn7h\
                       \.apps.googleusercontent.com"
                       "B6WSP2OTZso-QO9joatmKcU3"
                       "https://accounts.google.com/o/oauth2/auth"
                       "https://accounts.google.com/o/oauth2/token"
                       (Just "urn:ietf:wg:oauth:2.0:oob")
        url  = authorizationUrl oauth `appendQueryParam` googleScope
    pin <- scriptIO . runResourceT . getPIN "GMail" $ B8.unpack url
    manager <- scriptIO $ newManager tlsManagerSettings
    aToken <-  ExceptT
           $   first L8.unpack
           <$> fetchAccessToken manager oauth pin

    return $ config
           & D.loginGmail .~ Just (JsonToken aToken)

withConfig :: FilePath -> (LoginInfo s -> Script (LoginInfo s)) -> Script ()
withConfig configFile f = writeConfig configFile =<< f =<< readConfig configFile
