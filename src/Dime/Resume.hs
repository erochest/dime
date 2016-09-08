{-# LANGUAGE OverloadedStrings #-}


module Dime.Resume where


import           Control.Error
import           Control.Lens
import           Control.Monad
import           Control.Monad.Trans
import           Data.Aeson
import qualified Data.ByteString.Char8     as B8
import           Data.Foldable
import           Data.Time
import           Database.Persist
import           Network.OAuth.OAuth2

import           Dime.Google.Network.Utils
import           Dime.Types
import           Dime.Types.Fields


queueURL :: PostSource -> URI -> [GetParam] -> Google (Entity DownloadCache)
queueURL src uri params = do
    now <- liftIO getCurrentTime
    let uri' = B8.unpack uri
        ps   = JSONField params
        dl   = DownloadCache uri' ps src Nothing now Nothing Nothing
    liftSql $ insertEntity dl

nextURL :: Google (Maybe (Entity DownloadCache))
nextURL = runMaybeT $ do
    now <- liftIO getCurrentTime
    (Entity k _) <- MaybeT
                 .  liftSql
                 $  selectFirst [DownloadCacheStarted ==. Nothing] []
    Entity k <$> lift (liftSql $ updateGet k [DownloadCacheStarted =. Just now])

getCacheURL :: (FromJSON a, ToPostObject a)
            => ArchiveSessionId -> Entity DownloadCache -> Google a
getCacheURL sId (Entity dcId d) =
    maybe download return =<< runMaybeT (
        hoistMaybe . fromPostObject . unJSON . view postRaw
            =<< MaybeT . liftSql . get
            =<< hoistMaybe (d ^. downloadCachePostId)
        )
    where
        download = do
            now <- liftIO getCurrentTime
            a   <- getUncachedJSON (d ^. downloadCacheUrl    . to B8.pack)
                                   (d ^. downloadCacheParams . to unJSON)
            case toPostObject a of
                Just po -> do
                    p   <- liftSql
                        .  insertEntity
                        $  Post (getSource a) (getSourceId a) (fold $ getSender a)
                                (fold $ getMessage a) (JSONField $ getMetadata a)
                                (JSONField po) (getSent a) now sId
                    void $ liftSql
                         $ updateGet dcId [DownloadCachePostId =. Just (entityKey p)]
                    return a
                Nothing -> return a
