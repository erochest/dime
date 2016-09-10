{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}


module Dime.Resume where


import           Control.Error
import           Control.Lens
import           Control.Monad
import           Control.Monad.Trans
import           Data.Aeson
import qualified Data.ByteString.Char8 as B8
import           Data.Foldable
import           Data.Time
import           Database.Persist
import           Network.OAuth.OAuth2

import           Dime.Network.Utils
import           Dime.Types
import           Dime.Types.Fields


queueURL :: PostSource -> URI -> [GetParam] -> Dime (Entity DownloadCache)
queueURL src uri params = do
    now <- liftIO getCurrentTime
    let uri' = B8.unpack uri
        ps   = JSONField params
        dl   = DownloadCache uri' ps src Nothing now Nothing Nothing
    liftSql $ insertEntity dl

nextURL :: Dime (Maybe (Entity DownloadCache))
nextURL = runMaybeT $ do
    now <- liftIO getCurrentTime
    (Entity k _) <- MaybeT
                 .  liftSql
                 $  selectFirst [DownloadCacheStarted ==. Nothing] []
    Entity k <$> lift (liftSql $ updateGet k [DownloadCacheStarted =. Just now])

getCacheURL :: (FromJSON a, ToPostObject a)
            => ArchiveSessionId -> Entity DownloadCache -> Dime a
getCacheURL sId (Entity dcId d) =
    maybe download return =<< runMaybeT (
        hoistMaybe . fromPostObject . unJSON . view postRaw
            =<< MaybeT . liftSql . get
            =<< hoistMaybe (d ^. downloadCachePostId)
        )
    where
        download :: (FromJSON a, ToPostObject a) => Dime a
        download =   save
                 =<< getUncachedJSON (d ^. downloadCacheUrl    . to B8.pack)
                                     (d ^. downloadCacheParams . to unJSON)

        save :: ToPostObject po => po -> Dime po
        save po = do
            now <- liftIO getCurrentTime
            p   <- liftSql
                .  insertEntity
                $  Post (getSource po) (getSourceId po) (fold $ getSender po)
                        (fold $ getMessage po) (JSONField $ getMetadata po)
                        (JSONField $ toPostObject po) (getSent po) now sId
            void $ liftSql
                 $ updateGet dcId [DownloadCachePostId =. Just (entityKey p)]
            return po
