{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}


module Dime.Network where


import           Control.Lens            hiding ((??))
import           Control.Monad.Reader
import           Data.Aeson
import           Data.Foldable
import           Data.Time
import           Database.Persist.Sqlite hiding (get)
import           Network.OAuth.OAuth2
import           Network.Wreq
import           Network.Wreq.Types      hiding (auth, manager)

import           Dime.Network.Utils
import           Dime.Resume
import           Dime.Types
import           Dime.Types.Fields


getJSON :: (ToPostObject a, FromJSON a) => URI -> Dime a
getJSON uri = getJSON' uri []

getJSON' :: (ToPostObject a, FromJSON a) => URI -> [GetParam] -> Dime a
getJSON' uri ps = do
    src <- use dsSource
    dl  <- queueURL src uri ps
    s   <- maybe (newArchiveSession src) return =<< use dsSession
    getCacheURL (entityKey s) dl
    where
        newArchiveSession :: PostSource -> Dime (Entity ArchiveSession)
        newArchiveSession src = do
            now <- liftIO getCurrentTime
            s   <- liftSql . insertEntity $ ArchiveSession src now
            assign dsSession $ Just s
            return s

postJSON :: (Postable a, FromJSON b) => URI -> a -> Dime b
postJSON uri d = do
    (m, t) <- currentManagerToken
    let opts = defaults
             & manager .~ Right m
             & auth    ?~ oauth2Bearer (accessToken t)
    asJSON' =<< liftIO (postWith opts (normURL uri) d)

postJSON' :: (Postable a, FromJSON b)
          => URI -> [GetParam] -> a -> Dime b
postJSON' uri ps d = do
    (m, t) <- currentManagerToken
    let opts' = defaults
              & manager .~ Right m
              & auth ?~ oauth2Bearer (accessToken t)
        opts  = foldl' setp opts' ps
    asJSON' =<< liftIO (postWith opts (normURL uri) d)
