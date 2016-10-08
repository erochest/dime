{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Dialogue.Actions.Stats where


import           Control.Arrow
import           Control.Error
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson
import qualified Data.ByteString.Lazy    as BL
import           Data.Foldable
import           Data.Hashable
import qualified Data.HashMap.Strict     as M
import qualified Data.HashSet            as S
import           Data.Int
import qualified Data.List               as L
import           Data.Ord                (comparing)
import qualified Data.Sequence           as Seq
import qualified Data.Text               as T
import           Data.Time
import           Data.Tuple
import           Database.Persist
import           Database.Persist.Sql

import           Dialogue.Models
import           Dialogue.Types.Dialogue
import           Dialogue.Types.Stats
import           Dialogue.Utils


generateStats :: FilePath -> FilePath -> Script ()
generateStats dbFile outputFile = runDialogueS' (T.pack dbFile) $ do
    profiles <- liftSql $ selectList [] []
    handles  <- liftSql $ selectList [] []
    adium    :: [Entity AdiumMessage  ] <- liftSql $ selectList [] []
    google   :: [Entity GoogleMessage ] <- liftSql $ selectList [] []
    twitter  :: [Entity TwitterMessage] <- liftSql $ selectList [] []
    let profileHandles = M.fromList
                       $ map (   (fromSqlKey . entityKey)
                             &&& (fromSqlKey . _handleProfileId . entityVal)
                             ) handles
        profileIndex   = M.mapMaybe listToMaybe
                       $ indexBy (fromSqlKey . entityKey) profiles
        adiumIndex     = getStats profileHandles _adiumMessageSenderHandleId
                            _adiumMessageCreatedAt   adium
        googleIndex    = getStats profileHandles _googleMessageSenderId
                            _googleMessageCreatedAt  google
        twitterIndex   = getStats profileHandles _twitterMessageSenderHandleId
                            _twitterMessageCreatedAt twitter
    liftIO
        $ BL.writeFile outputFile
        $ encode
        $ L.sortBy (comparing (mcYear &&& mcMonth))
        $ statsToCounts profileIndex adiumIndex googleIndex twitterIndex

getStats :: forall record
         .  M.HashMap Int64 Int64
         -> (record -> HandleId)
         -> (record -> UTCTime)
         -> [Entity record]
         -> MonthIndex (ProfileIndex Int)
getStats profiles getHandle getDate xs =
    fmap length . indexByM handleKey <$> indexBy  dateKey xs
    where
        dateKey   = monthKey . getDate . entityVal
        handleKey = (`M.lookup` profiles) . fromSqlKey . getHandle . entityVal

indexBy :: (Eq b, Hashable b) => (a -> b) -> [a] -> M.HashMap b [a]
indexBy f = fmap toList . M.fromListWith mappend . fmap (f &&& Seq.singleton)

indexByM :: (Eq b, Hashable b) => (a -> Maybe b) -> [a] -> M.HashMap b [a]
indexByM f = fmap toList
           . M.fromListWith mappend
           . map swap
           . mapMaybe (sequenceA . (Seq.singleton &&& f))

statsToCounts :: M.HashMap Int64 (Entity Profile)
              -> MonthIndex (ProfileIndex Int)
              -> MonthIndex (ProfileIndex Int)
              -> MonthIndex (ProfileIndex Int)
              -> [MonthCount]
statsToCounts ps acs gcs tcs =
    fold $ do
        p0 <- getProfile (_profilePrimary . entityVal)       $ M.elems ps
        p1 <- getProfile (not . _profilePrimary . entityVal) $ M.elems ps
        let count0 = ServiceCount (ProfileCount (snd p0) 0)
                                  (ProfileCount (snd p1) 0)
        forM mnths $ \k@(year, month) ->
            return $ MonthCount year month
                   (getSC count0 p0 p1 k acs)
                   (getSC count0 p0 p1 k gcs)
                   (getSC count0 p0 p1 k tcs)
    where
        mnths = L.sort $ toList $ foldMap (S.fromList . M.keys) [acs, gcs, tcs]

        getSC c0 p0 p1 k m =
            fromMaybe c0 $ indexToPC p0 p1 =<< M.lookup k m

        indexToPC p0 p1 m =
            ServiceCount
            <$> fmap (ProfileCount (snd p0)) (M.lookup (fromSqlKey (fst p0)) m)
            <*> fmap (ProfileCount (snd p1)) (M.lookup (fromSqlKey (fst p1)) m)
