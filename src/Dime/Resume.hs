{-# LANGUAGE OverloadedStrings #-}


module Dime.Resume where


import           Control.Error
import           Control.Lens
import           Control.Monad.Trans
import           Data.Aeson
import qualified Data.ByteString.Char8     as B8
import           Data.Foldable
import           Data.Time
import           Database.Persist
import           Network.OAuth.OAuth2

import           Dime.Google.Network.Utils
import           Dime.Google.Types
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
            => ArchiveSessionId -> Entity DownloadCache
            -> Google (Entity DownloadCache, Entity Post, a)
getCacheURL sId ed@(Entity dcId d) =
    maybe download return =<< runMaybeT (do
        pId <- hoistMaybe $ d ^. downloadCachePostId
        p'  <- MaybeT . liftSql $ get pId
        po  <- hoistMaybe . fromPostObject . unJSON $ p' ^. postRaw
        return (ed, Entity pId p', po)
        )
    where
        download = do
            now <- liftIO getCurrentTime
            a   <- getUncachedJSON (d ^. downloadCacheUrl . to B8.pack)
                                   (d ^. downloadCacheParams . to unJSON)
            p   <- liftSql
                .  insertEntity
                $  Post (getSource a) (getSourceId a) (fold $ getSender a)
                        (fold $ getMessage a) (JSONField $ getMetadata a)
                        (JSONField $ toPostObject a) (getSent a) now sId
            d'  <- liftSql
                $  updateGet dcId [DownloadCachePostId =. Just (entityKey p)]
            return (Entity dcId d', p, a)
