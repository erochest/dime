{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}


module Dime.Network.Utils where


import           Control.Concurrent         (threadDelay)
import           Control.Error
import           Control.Exception          (displayException)
import           Control.Lens               hiding ((??))
import           Control.Monad.IO.Class
import           Data.Aeson
import qualified Data.ByteString.Char8      as B8
import qualified Data.ByteString.Lazy.Char8 as BL8
import           Data.Foldable
import qualified Data.Text                  as T
import           Network.OAuth.OAuth2
import           Network.Wreq

import           Dime.Types
import           Dime.Utils


requestWait :: Int
requestWait = 2000

baseURL :: URI
baseURL = "https://www.googleapis.com"

normURL :: URI -> String
normURL = B8.unpack . mappend baseURL

asJSON' :: FromJSON a => Response BL8.ByteString -> Dime a
asJSON' = liftE
        . hoistEither
        . (displayException `bimap` view responseBody)
        . asJSON

getUncachedJSON :: FromJSON a => URI -> [GetParam] -> Dime a
getUncachedJSON uri ps = do
    (m, t) <- currentManagerToken
    let opts' = defaults
              & manager .~ Right m
              & auth ?~ oauth2Bearer (accessToken t)
        opts  = foldl' setp opts' ps
    liftIO . threadDelay =<< watchM "waiting " requestWait
    asJSON' =<< liftIO (getWith opts (normURL uri))

maybeParam :: Show a => T.Text -> Maybe a -> Maybe GetParam
maybeParam n = fmap ((n,) . pure . T.pack . show)

maybeParamT :: T.Text -> Maybe T.Text -> Maybe GetParam
maybeParamT n = fmap ((n,) . pure)

maybeList :: T.Text -> [T.Text] -> Maybe GetParam
maybeList _ [] = Nothing
maybeList n vs = Just (n, vs)

setp :: Options -> GetParam -> Options
setp o (n, vs) = o & param n .~ vs
