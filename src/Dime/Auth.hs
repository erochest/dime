{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}


module Dime.Auth where


import           Control.Lens
import qualified Data.ByteString        as B
import           Network.OAuth.OAuth2
import           Web.Authenticate.OAuth
import           Web.Twitter.Conduit

import           Dime.Types


getTWInfo :: LoginInfo s -> Maybe TWInfo
getTWInfo LoginInfo{..} = do
    token  <- _loginTwitter ^? _Just . loginToken . _Just . ttokenToken
    secret <- _loginTwitter ^? _Just . loginToken . _Just . ttokenSecret
    let cred = Credential [ ("oauth_token", token)
                          , ("oauth_token_secret", secret)
                          ]
    return $ def { twToken = def { twOAuth = tokens, twCredential = cred }
                 , twProxy = Nothing
                 }
    where
        tokens = twitterOAuth
            { oauthConsumerKey    = _loginTwitter ^. _Just . loginKey . ckeyKey
            , oauthConsumerSecret =
                _loginTwitter ^. _Just . loginKey . ckeySecret
            }


googleScopeEmail, googleScopeRead, googleScopeInsert, googleScopeLabels
    , googleScope :: QueryParams

googleScopeEmail  = [("scope", "https://www.googleapis.com/auth/userinfo.email")]
googleScopeRead   = [("scope", "https://www.googleapis.com/auth/gmail.readonly")]
googleScopeInsert = [("scope", "https://www.googleapis.com/auth/gmail.insert")]
googleScopeLabels = [("scope", "https://www.googleapis.com/auth/gmail.labels")]
googleScope = pure
            . ("scope",)
            . joinScopes
            . fmap snd
            $ mconcat
            [ googleScopeEmail
            , googleScopeRead
            , googleScopeInsert
            , googleScopeLabels
            ]

joinScopes :: [B.ByteString] -> B.ByteString
joinScopes = B.intercalate " "
