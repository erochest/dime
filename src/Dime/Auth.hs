{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module Dime.Auth where


import           Control.Lens
import           Web.Authenticate.OAuth
import           Web.Twitter.Conduit

import           Dime.Types


getTWInfo :: LoginInfo -> Maybe TWInfo
getTWInfo LoginInfo{..} = do
    token  <- _loginToken ^? _Just . ttokenToken
    secret <- _loginToken ^? _Just . ttokenSecret
    let cred = Credential [ ("oauth_token", token)
                          , ("oauth_token_secret", secret)
                          ]
    return $ def { twToken = def { twOAuth = tokens, twCredential = cred }
                 , twProxy = Nothing
                 }
    where
        tokens = twitterOAuth
            { oauthConsumerKey    = _loginKey ^. ckeyKey
            , oauthConsumerSecret = _loginKey ^. ckeySecret
            }

